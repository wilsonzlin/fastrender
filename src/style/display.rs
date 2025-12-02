//! CSS Display property
//!
//! This module implements the CSS `display` property according to
//! CSS Display Module Level 3.
//!
//! The display property defines:
//! - Whether an element generates boxes at all
//! - What type of box is generated (block, inline, etc.)
//! - What formatting context the element establishes
//!
//! # Display Types
//!
//! Modern CSS has two-part display values:
//! - **Outer display**: How element participates in parent's FC (block/inline)
//! - **Inner display**: What FC element establishes for children (flow/flex/grid/table)
//!
//! # Examples
//!
//! ```
//! use fastrender::Display;
//!
//! let display = Display::parse("flex").unwrap();
//! assert!(display.is_block_level());
//! assert!(display.establishes_formatting_context());
//! ```

use std::fmt;

/// CSS display property value
///
/// Represents the display type of an element, controlling how it
/// participates in layout and what formatting context it establishes.
///
/// # Examples
///
/// ```
/// use fastrender::Display;
///
/// let block = Display::Block;
/// assert!(block.is_block_level());
///
/// let flex = Display::Flex;
/// assert!(flex.establishes_formatting_context());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Display {
    /// Element generates no boxes (removed from layout tree)
    ///
    /// Corresponds to `display: none`
    None,

    /// Block-level box, establishes block formatting context
    ///
    /// Corresponds to `display: block`
    /// - Outer: block-level
    /// - Inner: flow (block formatting context)
    Block,

    /// Inline-level box
    ///
    /// Corresponds to `display: inline`
    /// - Outer: inline-level
    /// - Inner: flow (participates in parent's inline formatting context)
    Inline,

    /// Inline-level box that establishes block formatting context
    ///
    /// Corresponds to `display: inline-block`
    /// - Outer: inline-level
    /// - Inner: flow-root (establishes new block formatting context)
    InlineBlock,

    /// Block-level flex container
    ///
    /// Corresponds to `display: flex`
    /// - Outer: block-level
    /// - Inner: flex
    Flex,

    /// Inline-level flex container
    ///
    /// Corresponds to `display: inline-flex`
    /// - Outer: inline-level
    /// - Inner: flex
    InlineFlex,

    /// Block-level grid container
    ///
    /// Corresponds to `display: grid`
    /// - Outer: block-level
    /// - Inner: grid
    Grid,

    /// Inline-level grid container
    ///
    /// Corresponds to `display: inline-grid`
    /// - Outer: inline-level
    /// - Inner: grid
    InlineGrid,

    /// Block-level table wrapper box
    ///
    /// Corresponds to `display: table`
    /// - Outer: block-level
    /// - Inner: table
    Table,

    /// Inline-level table wrapper box
    ///
    /// Corresponds to `display: inline-table`
    /// - Outer: inline-level
    /// - Inner: table
    InlineTable,

    /// Table row box
    ///
    /// Corresponds to `display: table-row`
    TableRow,

    /// Table cell box
    ///
    /// Corresponds to `display: table-cell`
    TableCell,

    /// Table row group box
    ///
    /// Corresponds to `display: table-row-group`
    TableRowGroup,

    /// Table header group box
    ///
    /// Corresponds to `display: table-header-group`
    TableHeaderGroup,

    /// Table footer group box
    ///
    /// Corresponds to `display: table-footer-group`
    TableFooterGroup,

    /// Table column box
    ///
    /// Corresponds to `display: table-column`
    TableColumn,

    /// Table column group box
    ///
    /// Corresponds to `display: table-column-group`
    TableColumnGroup,

    /// Table caption box
    ///
    /// Corresponds to `display: table-caption`
    TableCaption,

    /// List item box (block with marker)
    ///
    /// Corresponds to `display: list-item`
    /// - Outer: block-level
    /// - Inner: flow
    /// - Also generates marker box
    ListItem,

    /// Element's children are laid out as flex items, but element itself
    /// uses flow layout for text
    ///
    /// Corresponds to `display: flow-root`
    /// - Outer: block-level
    /// - Inner: flow-root (establishes new BFC)
    FlowRoot,

    /// Contents of element are promoted to parent
    ///
    /// Corresponds to `display: contents`
    /// Element generates no box, but children are laid out as if they were
    /// children of the element's parent
    Contents,
}

impl Display {
    /// Returns true if this display value means the element generates no boxes
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Display;
    ///
    /// assert!(Display::None.is_none());
    /// assert!(!Display::Block.is_none());
    /// ```
    pub fn is_none(self) -> bool {
        matches!(self, Display::None)
    }

    /// Returns true if the element generates block-level boxes
    ///
    /// Block-level boxes participate in a block formatting context.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Display;
    ///
    /// assert!(Display::Block.is_block_level());
    /// assert!(Display::Flex.is_block_level());
    /// assert!(Display::Grid.is_block_level());
    /// assert!(Display::Table.is_block_level());
    /// assert!(!Display::Inline.is_block_level());
    /// assert!(!Display::InlineFlex.is_block_level());
    /// ```
    pub fn is_block_level(self) -> bool {
        matches!(
            self,
            Display::Block | Display::Flex | Display::Grid | Display::Table | Display::ListItem | Display::FlowRoot
        )
    }

    /// Returns true if the element generates inline-level boxes
    ///
    /// Inline-level boxes participate in an inline formatting context.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Display;
    ///
    /// assert!(Display::Inline.is_inline_level());
    /// assert!(Display::InlineBlock.is_inline_level());
    /// assert!(Display::InlineFlex.is_inline_level());
    /// assert!(Display::InlineGrid.is_inline_level());
    /// assert!(!Display::Block.is_inline_level());
    /// ```
    pub fn is_inline_level(self) -> bool {
        matches!(
            self,
            Display::Inline | Display::InlineBlock | Display::InlineFlex | Display::InlineGrid | Display::InlineTable
        )
    }

    /// Returns true if the element is a table-internal box
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Display;
    ///
    /// assert!(Display::TableRow.is_table_internal());
    /// assert!(Display::TableCell.is_table_internal());
    /// assert!(!Display::Table.is_table_internal());
    /// ```
    pub fn is_table_internal(self) -> bool {
        matches!(
            self,
            Display::TableRow
                | Display::TableCell
                | Display::TableRowGroup
                | Display::TableHeaderGroup
                | Display::TableFooterGroup
                | Display::TableColumn
                | Display::TableColumnGroup
                | Display::TableCaption
        )
    }

    /// Returns true if this display value establishes a new formatting context
    ///
    /// Elements that establish formatting contexts contain their floats,
    /// margins don't collapse through them, etc.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Display;
    ///
    /// assert!(Display::Flex.establishes_formatting_context());
    /// assert!(Display::Grid.establishes_formatting_context());
    /// assert!(Display::InlineBlock.establishes_formatting_context());
    /// assert!(Display::FlowRoot.establishes_formatting_context());
    /// assert!(!Display::Inline.establishes_formatting_context());
    /// ```
    pub fn establishes_formatting_context(self) -> bool {
        matches!(
            self,
            Display::InlineBlock
                | Display::Flex
                | Display::InlineFlex
                | Display::Grid
                | Display::InlineGrid
                | Display::Table
                | Display::InlineTable
                | Display::FlowRoot
        )
    }

    /// Returns the type of formatting context this element establishes
    ///
    /// Returns None if the element doesn't establish a formatting context.
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::{Display, FormattingContextType};
    ///
    /// assert_eq!(
    ///     Display::Flex.formatting_context_type(),
    ///     Some(FormattingContextType::Flex)
    /// );
    /// assert_eq!(
    ///     Display::Grid.formatting_context_type(),
    ///     Some(FormattingContextType::Grid)
    /// );
    /// assert_eq!(Display::Inline.formatting_context_type(), None);
    /// ```
    pub fn formatting_context_type(self) -> Option<FormattingContextType> {
        match self {
            Display::Block => Some(FormattingContextType::Block),
            Display::InlineBlock => Some(FormattingContextType::Block),
            Display::FlowRoot => Some(FormattingContextType::Block),
            Display::Flex | Display::InlineFlex => Some(FormattingContextType::Flex),
            Display::Grid | Display::InlineGrid => Some(FormattingContextType::Grid),
            Display::Table | Display::InlineTable => Some(FormattingContextType::Table),
            _ => None,
        }
    }

    /// Returns the outer display type
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use fastrender::style::{Display, OuterDisplay};
    ///
    /// assert_eq!(Display::Block.outer_display(), OuterDisplay::Block);
    /// assert_eq!(Display::Inline.outer_display(), OuterDisplay::Inline);
    /// assert_eq!(Display::InlineFlex.outer_display(), OuterDisplay::Inline);
    /// ```
    pub fn outer_display(self) -> OuterDisplay {
        match self {
            Display::None => OuterDisplay::None,
            Display::Inline
            | Display::InlineBlock
            | Display::InlineFlex
            | Display::InlineGrid
            | Display::InlineTable => OuterDisplay::Inline,
            _ => OuterDisplay::Block,
        }
    }

    /// Returns the inner display type
    ///
    /// # Examples
    ///
    /// ```ignore
    /// use fastrender::style::{Display, InnerDisplay};
    ///
    /// assert_eq!(Display::Block.inner_display(), InnerDisplay::Flow);
    /// assert_eq!(Display::Flex.inner_display(), InnerDisplay::Flex);
    /// assert_eq!(Display::Grid.inner_display(), InnerDisplay::Grid);
    /// ```
    pub fn inner_display(self) -> InnerDisplay {
        match self {
            Display::None => InnerDisplay::None,
            Display::Flex | Display::InlineFlex => InnerDisplay::Flex,
            Display::Grid | Display::InlineGrid => InnerDisplay::Grid,
            Display::Table | Display::InlineTable => InnerDisplay::Table,
            Display::FlowRoot | Display::InlineBlock => InnerDisplay::FlowRoot,
            _ => InnerDisplay::Flow,
        }
    }

    /// Parse a display value from a CSS string
    ///
    /// # Examples
    ///
    /// ```
    /// use fastrender::Display;
    ///
    /// assert_eq!(Display::parse("block").unwrap(), Display::Block);
    /// assert_eq!(Display::parse("flex").unwrap(), Display::Flex);
    /// assert_eq!(Display::parse("inline-block").unwrap(), Display::InlineBlock);
    /// assert!(Display::parse("invalid").is_err());
    /// ```
    pub fn parse(s: &str) -> Result<Self, DisplayParseError> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "none" => Ok(Display::None),
            "block" => Ok(Display::Block),
            "inline" => Ok(Display::Inline),
            "inline-block" => Ok(Display::InlineBlock),
            "flex" => Ok(Display::Flex),
            "inline-flex" => Ok(Display::InlineFlex),
            "grid" => Ok(Display::Grid),
            "inline-grid" => Ok(Display::InlineGrid),
            "table" => Ok(Display::Table),
            "inline-table" => Ok(Display::InlineTable),
            "table-row" => Ok(Display::TableRow),
            "table-cell" => Ok(Display::TableCell),
            "table-row-group" => Ok(Display::TableRowGroup),
            "table-header-group" => Ok(Display::TableHeaderGroup),
            "table-footer-group" => Ok(Display::TableFooterGroup),
            "table-column" => Ok(Display::TableColumn),
            "table-column-group" => Ok(Display::TableColumnGroup),
            "table-caption" => Ok(Display::TableCaption),
            "list-item" => Ok(Display::ListItem),
            "flow-root" => Ok(Display::FlowRoot),
            "contents" => Ok(Display::Contents),
            _ => Err(DisplayParseError::InvalidValue(s.to_string())),
        }
    }
}

/// Outer display type (how element participates in parent's formatting context)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OuterDisplay {
    /// No box generated
    None,
    /// Block-level box
    Block,
    /// Inline-level box
    Inline,
}

/// Inner display type (what formatting context element establishes)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InnerDisplay {
    /// No formatting context
    None,
    /// Normal flow (block formatting context)
    Flow,
    /// Establishes new block formatting context
    FlowRoot,
    /// Flex formatting context
    Flex,
    /// Grid formatting context
    Grid,
    /// Table formatting context
    Table,
}

/// Formatting context types
///
/// A formatting context defines how child boxes are laid out.
///
/// Reference: CSS Display Module Level 3
/// <https://www.w3.org/TR/css-display-3/#formatting-context>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormattingContextType {
    /// Block formatting context (BFC)
    ///
    /// Block-level boxes are laid out vertically, one after another.
    /// Margins collapse. This is the default for most block containers.
    Block,

    /// Inline formatting context (IFC)
    ///
    /// Inline-level boxes are laid out horizontally within lines.
    /// Text wraps at line boundaries.
    Inline,

    /// Flex formatting context
    ///
    /// Children are laid out using the flexbox algorithm.
    Flex,

    /// Grid formatting context
    ///
    /// Children are positioned in a 2D grid.
    Grid,

    /// Table formatting context
    ///
    /// Children are laid out as table rows/columns.
    Table,
}

/// Error when parsing display value
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DisplayParseError {
    /// Invalid display value
    InvalidValue(String),
}

impl fmt::Display for DisplayParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DisplayParseError::InvalidValue(s) => {
                write!(f, "Invalid display value: '{}'", s)
            }
        }
    }
}

impl std::error::Error for DisplayParseError {}

impl fmt::Display for Display {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Display::None => write!(f, "none"),
            Display::Block => write!(f, "block"),
            Display::Inline => write!(f, "inline"),
            Display::InlineBlock => write!(f, "inline-block"),
            Display::Flex => write!(f, "flex"),
            Display::InlineFlex => write!(f, "inline-flex"),
            Display::Grid => write!(f, "grid"),
            Display::InlineGrid => write!(f, "inline-grid"),
            Display::Table => write!(f, "table"),
            Display::InlineTable => write!(f, "inline-table"),
            Display::TableRow => write!(f, "table-row"),
            Display::TableCell => write!(f, "table-cell"),
            Display::TableRowGroup => write!(f, "table-row-group"),
            Display::TableHeaderGroup => write!(f, "table-header-group"),
            Display::TableFooterGroup => write!(f, "table-footer-group"),
            Display::TableColumn => write!(f, "table-column"),
            Display::TableColumnGroup => write!(f, "table-column-group"),
            Display::TableCaption => write!(f, "table-caption"),
            Display::ListItem => write!(f, "list-item"),
            Display::FlowRoot => write!(f, "flow-root"),
            Display::Contents => write!(f, "contents"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Basic parsing tests
    #[test]
    fn test_parse_block() {
        assert_eq!(Display::parse("block").unwrap(), Display::Block);
    }

    #[test]
    fn test_parse_inline() {
        assert_eq!(Display::parse("inline").unwrap(), Display::Inline);
    }

    #[test]
    fn test_parse_none() {
        assert_eq!(Display::parse("none").unwrap(), Display::None);
    }

    #[test]
    fn test_parse_flex() {
        assert_eq!(Display::parse("flex").unwrap(), Display::Flex);
    }

    #[test]
    fn test_parse_inline_flex() {
        assert_eq!(Display::parse("inline-flex").unwrap(), Display::InlineFlex);
    }

    #[test]
    fn test_parse_grid() {
        assert_eq!(Display::parse("grid").unwrap(), Display::Grid);
    }

    #[test]
    fn test_parse_inline_grid() {
        assert_eq!(Display::parse("inline-grid").unwrap(), Display::InlineGrid);
    }

    #[test]
    fn test_parse_table() {
        assert_eq!(Display::parse("table").unwrap(), Display::Table);
    }

    #[test]
    fn test_parse_inline_table() {
        assert_eq!(Display::parse("inline-table").unwrap(), Display::InlineTable);
    }

    #[test]
    fn test_parse_table_cell() {
        assert_eq!(Display::parse("table-cell").unwrap(), Display::TableCell);
    }

    #[test]
    fn test_parse_table_row() {
        assert_eq!(Display::parse("table-row").unwrap(), Display::TableRow);
    }

    #[test]
    fn test_parse_list_item() {
        assert_eq!(Display::parse("list-item").unwrap(), Display::ListItem);
    }

    #[test]
    fn test_parse_flow_root() {
        assert_eq!(Display::parse("flow-root").unwrap(), Display::FlowRoot);
    }

    #[test]
    fn test_parse_contents() {
        assert_eq!(Display::parse("contents").unwrap(), Display::Contents);
    }

    #[test]
    fn test_parse_case_insensitive() {
        assert_eq!(Display::parse("BLOCK").unwrap(), Display::Block);
        assert_eq!(Display::parse("Inline-Block").unwrap(), Display::InlineBlock);
        assert_eq!(Display::parse("INLINE-FLEX").unwrap(), Display::InlineFlex);
    }

    #[test]
    fn test_parse_with_whitespace() {
        assert_eq!(Display::parse("  block  ").unwrap(), Display::Block);
        assert_eq!(Display::parse("\tflex\n").unwrap(), Display::Flex);
    }

    #[test]
    fn test_parse_invalid() {
        assert!(Display::parse("invalid").is_err());
        assert!(Display::parse("").is_err());
        assert!(Display::parse("block-inline").is_err());
    }

    // is_none tests
    #[test]
    fn test_is_none() {
        assert!(Display::None.is_none());
        assert!(!Display::Block.is_none());
        assert!(!Display::Inline.is_none());
        assert!(!Display::Flex.is_none());
    }

    // is_block_level tests
    #[test]
    fn test_is_block_level() {
        assert!(Display::Block.is_block_level());
        assert!(Display::Flex.is_block_level());
        assert!(Display::Grid.is_block_level());
        assert!(Display::Table.is_block_level());
        assert!(Display::ListItem.is_block_level());
        assert!(Display::FlowRoot.is_block_level());

        assert!(!Display::Inline.is_block_level());
        assert!(!Display::InlineBlock.is_block_level());
        assert!(!Display::InlineFlex.is_block_level());
        assert!(!Display::InlineGrid.is_block_level());
        assert!(!Display::InlineTable.is_block_level());
        assert!(!Display::None.is_block_level());
    }

    // is_inline_level tests
    #[test]
    fn test_is_inline_level() {
        assert!(Display::Inline.is_inline_level());
        assert!(Display::InlineBlock.is_inline_level());
        assert!(Display::InlineFlex.is_inline_level());
        assert!(Display::InlineGrid.is_inline_level());
        assert!(Display::InlineTable.is_inline_level());

        assert!(!Display::Block.is_inline_level());
        assert!(!Display::Flex.is_inline_level());
        assert!(!Display::Grid.is_inline_level());
        assert!(!Display::Table.is_inline_level());
        assert!(!Display::None.is_inline_level());
    }

    // is_table_internal tests
    #[test]
    fn test_is_table_internal() {
        assert!(Display::TableRow.is_table_internal());
        assert!(Display::TableCell.is_table_internal());
        assert!(Display::TableRowGroup.is_table_internal());
        assert!(Display::TableHeaderGroup.is_table_internal());
        assert!(Display::TableFooterGroup.is_table_internal());
        assert!(Display::TableColumn.is_table_internal());
        assert!(Display::TableColumnGroup.is_table_internal());
        assert!(Display::TableCaption.is_table_internal());

        assert!(!Display::Table.is_table_internal());
        assert!(!Display::InlineTable.is_table_internal());
        assert!(!Display::Block.is_table_internal());
    }

    // establishes_formatting_context tests
    #[test]
    fn test_establishes_formatting_context() {
        assert!(Display::Flex.establishes_formatting_context());
        assert!(Display::InlineFlex.establishes_formatting_context());
        assert!(Display::Grid.establishes_formatting_context());
        assert!(Display::InlineGrid.establishes_formatting_context());
        assert!(Display::InlineBlock.establishes_formatting_context());
        assert!(Display::Table.establishes_formatting_context());
        assert!(Display::InlineTable.establishes_formatting_context());
        assert!(Display::FlowRoot.establishes_formatting_context());

        assert!(!Display::Block.establishes_formatting_context());
        assert!(!Display::Inline.establishes_formatting_context());
        assert!(!Display::None.establishes_formatting_context());
    }

    // formatting_context_type tests
    #[test]
    fn test_formatting_context_type() {
        assert_eq!(
            Display::Block.formatting_context_type(),
            Some(FormattingContextType::Block)
        );
        assert_eq!(
            Display::InlineBlock.formatting_context_type(),
            Some(FormattingContextType::Block)
        );
        assert_eq!(
            Display::FlowRoot.formatting_context_type(),
            Some(FormattingContextType::Block)
        );
        assert_eq!(
            Display::Flex.formatting_context_type(),
            Some(FormattingContextType::Flex)
        );
        assert_eq!(
            Display::InlineFlex.formatting_context_type(),
            Some(FormattingContextType::Flex)
        );
        assert_eq!(
            Display::Grid.formatting_context_type(),
            Some(FormattingContextType::Grid)
        );
        assert_eq!(
            Display::InlineGrid.formatting_context_type(),
            Some(FormattingContextType::Grid)
        );
        assert_eq!(
            Display::Table.formatting_context_type(),
            Some(FormattingContextType::Table)
        );
        assert_eq!(
            Display::InlineTable.formatting_context_type(),
            Some(FormattingContextType::Table)
        );
        assert_eq!(Display::Inline.formatting_context_type(), None);
        assert_eq!(Display::None.formatting_context_type(), None);
    }

    // outer_display tests
    #[test]
    fn test_outer_display() {
        assert_eq!(Display::Block.outer_display(), OuterDisplay::Block);
        assert_eq!(Display::Flex.outer_display(), OuterDisplay::Block);
        assert_eq!(Display::Grid.outer_display(), OuterDisplay::Block);
        assert_eq!(Display::Table.outer_display(), OuterDisplay::Block);
        assert_eq!(Display::ListItem.outer_display(), OuterDisplay::Block);
        assert_eq!(Display::FlowRoot.outer_display(), OuterDisplay::Block);

        assert_eq!(Display::Inline.outer_display(), OuterDisplay::Inline);
        assert_eq!(Display::InlineBlock.outer_display(), OuterDisplay::Inline);
        assert_eq!(Display::InlineFlex.outer_display(), OuterDisplay::Inline);
        assert_eq!(Display::InlineGrid.outer_display(), OuterDisplay::Inline);
        assert_eq!(Display::InlineTable.outer_display(), OuterDisplay::Inline);

        assert_eq!(Display::None.outer_display(), OuterDisplay::None);
    }

    // inner_display tests
    #[test]
    fn test_inner_display() {
        assert_eq!(Display::Block.inner_display(), InnerDisplay::Flow);
        assert_eq!(Display::Inline.inner_display(), InnerDisplay::Flow);
        assert_eq!(Display::ListItem.inner_display(), InnerDisplay::Flow);

        assert_eq!(Display::Flex.inner_display(), InnerDisplay::Flex);
        assert_eq!(Display::InlineFlex.inner_display(), InnerDisplay::Flex);

        assert_eq!(Display::Grid.inner_display(), InnerDisplay::Grid);
        assert_eq!(Display::InlineGrid.inner_display(), InnerDisplay::Grid);

        assert_eq!(Display::Table.inner_display(), InnerDisplay::Table);
        assert_eq!(Display::InlineTable.inner_display(), InnerDisplay::Table);

        assert_eq!(Display::FlowRoot.inner_display(), InnerDisplay::FlowRoot);
        assert_eq!(Display::InlineBlock.inner_display(), InnerDisplay::FlowRoot);

        assert_eq!(Display::None.inner_display(), InnerDisplay::None);
    }

    // Display trait tests
    #[test]
    fn test_display_formatting() {
        assert_eq!(format!("{}", Display::Block), "block");
        assert_eq!(format!("{}", Display::Inline), "inline");
        assert_eq!(format!("{}", Display::InlineFlex), "inline-flex");
        assert_eq!(format!("{}", Display::TableCell), "table-cell");
        assert_eq!(format!("{}", Display::FlowRoot), "flow-root");
        assert_eq!(format!("{}", Display::Contents), "contents");
    }

    // Round-trip test
    #[test]
    fn test_parse_display_roundtrip() {
        let values = vec![
            Display::None,
            Display::Block,
            Display::Inline,
            Display::InlineBlock,
            Display::Flex,
            Display::InlineFlex,
            Display::Grid,
            Display::InlineGrid,
            Display::Table,
            Display::InlineTable,
            Display::TableRow,
            Display::TableCell,
            Display::TableRowGroup,
            Display::TableHeaderGroup,
            Display::TableFooterGroup,
            Display::TableColumn,
            Display::TableColumnGroup,
            Display::TableCaption,
            Display::ListItem,
            Display::FlowRoot,
            Display::Contents,
        ];

        for display in values {
            let string = format!("{}", display);
            let parsed = Display::parse(&string).unwrap();
            assert_eq!(parsed, display, "Round-trip failed for {:?}", display);
        }
    }

    // Error message test
    #[test]
    fn test_parse_error_message() {
        let err = Display::parse("invalid").unwrap_err();
        assert_eq!(err.to_string(), "Invalid display value: 'invalid'");
    }

    // Edge cases
    #[test]
    fn test_all_table_types() {
        assert_eq!(Display::parse("table-row-group").unwrap(), Display::TableRowGroup);
        assert_eq!(Display::parse("table-header-group").unwrap(), Display::TableHeaderGroup);
        assert_eq!(Display::parse("table-footer-group").unwrap(), Display::TableFooterGroup);
        assert_eq!(Display::parse("table-column").unwrap(), Display::TableColumn);
        assert_eq!(Display::parse("table-column-group").unwrap(), Display::TableColumnGroup);
        assert_eq!(Display::parse("table-caption").unwrap(), Display::TableCaption);
    }
}
