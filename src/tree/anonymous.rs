//! Anonymous box creation
//!
//! Implements CSS anonymous box generation rules to ensure the box tree
//! satisfies CSS structural constraints.
//!
//! # CSS Specifications
//!
//! - CSS 2.1 Section 9.2.1.1: Anonymous block boxes
//! - CSS 2.1 Section 9.2.2.1: Anonymous inline boxes
//! - CSS 2.1 Section 17.2.1: Anonymous table boxes
//!
//! # Anonymous Box Types
//!
//! ## Anonymous Block Boxes
//!
//! When a block container has both block-level and inline-level children,
//! the inline-level children are wrapped in anonymous block boxes.
//!
//! ```text
//! Before:                     After:
//! Block                       Block
//! ├── Text "Hello"            ├── AnonymousBlock
//! ├── Block (p)               │   └── Text "Hello"
//! └── Text "World"            ├── Block (p)
//!                             └── AnonymousBlock
//!                                 └── Text "World"
//! ```
//!
//! ## Anonymous Inline Boxes
//!
//! Text that isn't inside an inline element is wrapped in anonymous inline boxes.
//! This ensures all text has a containing inline box.
//!
//! # Usage
//!
//! ```ignore
//! use fastrender::tree::anonymous::AnonymousBoxCreator;
//!
//! let box_tree = generator.generate(&dom)?;
//! let fixed_tree = AnonymousBoxCreator::fixup_tree(box_tree);
//! ```

use crate::style::ComputedStyle;
use crate::tree::box_tree::{AnonymousBox, AnonymousType, BoxNode, BoxType};
use std::sync::Arc;

/// Creates anonymous boxes in a box tree
///
/// This struct provides static methods to transform a box tree by inserting
/// anonymous boxes where CSS structural rules require them.
///
/// # CSS 2.1 Section 9.2.1.1
///
/// "If a block container box has a block-level box inside it, then we force
/// it to have only block-level boxes inside it."
///
/// This is accomplished by wrapping runs of inline-level content in anonymous
/// block boxes.
pub struct AnonymousBoxCreator;

impl AnonymousBoxCreator {
    /// Fixes up a box tree by inserting anonymous boxes
    ///
    /// This is a post-processing step after initial box generation. It traverses
    /// the tree recursively and inserts anonymous boxes where required by CSS rules.
    ///
    /// # Algorithm
    ///
    /// 1. Recursively process all children first (bottom-up)
    /// 2. Then fix up this node's children based on its type
    /// 3. Block containers get block fixup (wrap inline runs)
    /// 4. Inline containers get inline fixup (wrap text nodes)
    ///
    /// # CSS 2.1 Section 9.2.1.1
    ///
    /// "If a block container box has a block-level box inside it, then we force
    /// it to have only block-level boxes inside it."
    ///
    /// # Examples
    ///
    /// ```
    /// use std::sync::Arc;
    /// use fastrender::tree::{BoxNode, FormattingContextType};
    /// use fastrender::tree::anonymous::AnonymousBoxCreator;
    /// use fastrender::tree::box_tree::ComputedStyle;
    ///
    /// let style = Arc::new(ComputedStyle::default());
    /// let text = BoxNode::new_text(style.clone(), "Hello".to_string());
    /// let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    /// let container = BoxNode::new_block(
    ///     style.clone(),
    ///     FormattingContextType::Block,
    ///     vec![text, block],
    /// );
    ///
    /// let fixed = AnonymousBoxCreator::fixup_tree(container);
    /// // Text is now wrapped in anonymous block
    /// assert_eq!(fixed.children.len(), 2);
    /// assert!(fixed.children[0].is_anonymous());
    /// ```
    pub fn fixup_tree(mut box_node: BoxNode) -> BoxNode {
        // First, recursively fix children (bottom-up traversal)
        box_node.children = box_node.children.into_iter().map(Self::fixup_tree).collect();

        // Then fix this node's children based on its type
        box_node.children = Self::fixup_children(box_node.children, &box_node.box_type);

        box_node
    }

    /// Fixes up children based on parent box type
    ///
    /// Dispatches to appropriate fixup method based on whether parent is
    /// a block container or inline container.
    fn fixup_children(children: Vec<BoxNode>, parent_type: &BoxType) -> Vec<BoxNode> {
        if children.is_empty() {
            return children;
        }

        match parent_type {
            // Block containers need block fixup
            BoxType::Block(_) => Self::fixup_block_children(children),

            // Inline containers need inline fixup
            BoxType::Inline(_) => Self::fixup_inline_children(children),

            // Anonymous block containers also need block fixup
            BoxType::Anonymous(anon)
                if matches!(anon.anonymous_type, AnonymousType::Block | AnonymousType::TableCell) =>
            {
                Self::fixup_block_children(children)
            }

            // Anonymous inline containers need inline fixup
            BoxType::Anonymous(anon) if matches!(anon.anonymous_type, AnonymousType::Inline) => {
                Self::fixup_inline_children(children)
            }

            // Text boxes, replaced elements, and others don't need fixup
            _ => children,
        }
    }

    /// Fixes up children of block containers
    ///
    /// CSS 2.1 Section 9.2.1.1: Block containers can only contain:
    /// - All block-level boxes, OR
    /// - All inline-level boxes (establishes inline formatting context)
    ///
    /// If mixed, we create anonymous block boxes to wrap inline content.
    ///
    /// # Algorithm
    ///
    /// 1. Check if children are all block-level, all inline-level, or mixed
    /// 2. If all block-level: no changes needed
    /// 3. If all inline-level: wrap any bare text in anonymous inline boxes
    /// 4. If mixed: wrap runs of inline content in anonymous block boxes
    fn fixup_block_children(children: Vec<BoxNode>) -> Vec<BoxNode> {
        // Determine what kind of content we have
        let has_block = children.iter().any(|c| c.is_block_level());
        let has_inline = children.iter().any(|c| c.is_inline_level());

        if has_block && has_inline {
            // Mixed content - wrap inline runs in anonymous blocks
            Self::wrap_inline_runs_in_anonymous_blocks(children)
        } else if !has_block && has_inline {
            // All inline content - wrap bare text in anonymous inlines
            // (This maintains proper inline structure)
            Self::wrap_bare_text_in_anonymous_inline(children)
        } else {
            // All block or empty - no fixup needed
            children
        }
    }

    /// Fixes up children of inline containers
    ///
    /// Inline boxes should only contain inline-level content.
    /// Bare text nodes are wrapped in anonymous inline boxes.
    ///
    /// CSS 2.1 Section 9.2.2.1: "Any text that is directly contained inside
    /// a block container element (not inside an inline element) must be
    /// treated as an anonymous inline element."
    fn fixup_inline_children(children: Vec<BoxNode>) -> Vec<BoxNode> {
        // Wrap bare text nodes in anonymous inline boxes
        children
            .into_iter()
            .map(|child| {
                if child.is_text() {
                    Self::create_anonymous_inline(child.style.clone(), vec![child])
                } else {
                    child
                }
            })
            .collect()
    }

    /// Wraps consecutive inline boxes in anonymous block boxes
    ///
    /// When a block container has mixed block/inline content, this function
    /// groups consecutive inline-level children and wraps each group in an
    /// anonymous block box.
    ///
    /// # Example
    ///
    /// ```text
    /// Input:  [Text, Text, Block, Inline, Block]
    /// Output: [AnonymousBlock[Text, Text], Block, AnonymousBlock[Inline], Block]
    /// ```
    ///
    /// Consecutive inline elements are wrapped together in a single anonymous
    /// block, preserving the document order.
    fn wrap_inline_runs_in_anonymous_blocks(children: Vec<BoxNode>) -> Vec<BoxNode> {
        let mut result = Vec::new();
        let mut inline_run: Vec<BoxNode> = Vec::new();

        for child in children {
            if child.is_inline_level() {
                // Accumulate inline boxes
                inline_run.push(child);
            } else {
                // Block box encountered - flush any inline run
                if !inline_run.is_empty() {
                    let anon_block = Self::create_anonymous_block_from_run(&mut inline_run);
                    result.push(anon_block);
                }
                result.push(child);
            }
        }

        // Flush remaining inline run at end
        if !inline_run.is_empty() {
            let anon_block = Self::create_anonymous_block_from_run(&mut inline_run);
            result.push(anon_block);
        }

        result
    }

    /// Wraps bare text nodes in anonymous inline boxes
    ///
    /// CSS 2.1 Section 9.2.2.1: Text directly inside a block container
    /// that isn't inside an inline element gets an anonymous inline box.
    fn wrap_bare_text_in_anonymous_inline(children: Vec<BoxNode>) -> Vec<BoxNode> {
        children
            .into_iter()
            .map(|child| {
                if child.is_text() {
                    // Wrap text in anonymous inline
                    Self::create_anonymous_inline(child.style.clone(), vec![child])
                } else {
                    child
                }
            })
            .collect()
    }

    /// Creates an anonymous block box from a run of inline boxes
    ///
    /// Takes ownership of the inline run and clears it.
    fn create_anonymous_block_from_run(inline_run: &mut Vec<BoxNode>) -> BoxNode {
        // Get style from first child (anonymous boxes inherit from parent,
        // but we need a style reference - use default for now)
        let style = if let Some(first) = inline_run.first() {
            first.style.clone()
        } else {
            Arc::new(ComputedStyle::default())
        };

        let children = std::mem::take(inline_run);
        Self::create_anonymous_block(style, children)
    }

    /// Creates an anonymous block box
    ///
    /// Anonymous block boxes are generated to satisfy the CSS constraint that
    /// block containers must contain either all block-level or all inline-level
    /// children.
    ///
    /// # Style Inheritance
    ///
    /// Anonymous boxes inherit computed style from their containing block.
    /// Currently uses default style as a placeholder - proper inheritance
    /// should be handled during style computation.
    pub fn create_anonymous_block(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::Block,
            }),
            children,
            debug_info: None,
        }
    }

    /// Creates an anonymous inline box
    ///
    /// Anonymous inline boxes wrap text that isn't inside an inline element.
    ///
    /// CSS 2.1 Section 9.2.2.1: "Any text that is directly contained inside
    /// a block container element (not inside an inline element) must be
    /// treated as an anonymous inline element."
    pub fn create_anonymous_inline(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::Inline,
            }),
            children,
            debug_info: None,
        }
    }

    /// Creates an anonymous table row box
    ///
    /// Used when table cells appear outside of table rows.
    pub fn create_anonymous_table_row(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableRow,
            }),
            children,
            debug_info: None,
        }
    }

    /// Creates an anonymous table cell box
    ///
    /// Used when non-table content appears inside table rows.
    pub fn create_anonymous_table_cell(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> BoxNode {
        BoxNode {
            style,
            box_type: BoxType::Anonymous(AnonymousBox {
                anonymous_type: AnonymousType::TableCell,
            }),
            children,
            debug_info: None,
        }
    }

    /// Checks if a list of children contains mixed block/inline content
    ///
    /// Returns true if children contain both block-level and inline-level boxes.
    pub fn has_mixed_content(children: &[BoxNode]) -> bool {
        let has_block = children.iter().any(|c| c.is_block_level());
        let has_inline = children.iter().any(|c| c.is_inline_level());
        has_block && has_inline
    }

    /// Checks if all children are block-level
    pub fn all_block_level(children: &[BoxNode]) -> bool {
        children.iter().all(|c| c.is_block_level())
    }

    /// Checks if all children are inline-level
    pub fn all_inline_level(children: &[BoxNode]) -> bool {
        children.iter().all(|c| c.is_inline_level())
    }

    /// Counts the number of anonymous boxes that would be created
    ///
    /// Useful for debugging and testing to verify fixup behavior.
    pub fn count_anonymous_boxes(node: &BoxNode) -> usize {
        let self_count = usize::from(node.is_anonymous());
        let children_count: usize = node.children.iter().map(Self::count_anonymous_boxes).sum();
        self_count + children_count
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::FormattingContextType;

    fn default_style() -> Arc<ComputedStyle> {
        Arc::new(ComputedStyle::default())
    }

    #[test]
    fn test_empty_container_no_crash() {
        let empty_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let fixed = AnonymousBoxCreator::fixup_tree(empty_block);
        assert_eq!(fixed.children.len(), 0);
    }

    #[test]
    fn test_all_block_children_no_change() {
        let child1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let child2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![child1, child2]);

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        // Should still have 2 children, no wrapping
        assert_eq!(fixed.children.len(), 2);
        assert!(!fixed.children[0].is_anonymous());
        assert!(!fixed.children[1].is_anonymous());
    }

    #[test]
    fn test_single_text_wrapped() {
        let text = BoxNode::new_text(default_style(), "Hello".to_string());

        let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        // Text should be wrapped in anonymous inline
        assert_eq!(fixed.children.len(), 1);
        assert!(fixed.children[0].is_anonymous());
        assert!(fixed.children[0].is_inline_level());
    }

    #[test]
    fn test_mixed_content_wraps_inline_runs() {
        let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());

        let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text1, block, text2]);

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        // Should have 3 children: anon block, block, anon block
        assert_eq!(fixed.children.len(), 3);

        // First child: anonymous block wrapping text1
        assert!(fixed.children[0].is_anonymous());
        assert!(fixed.children[0].is_block_level());
        assert_eq!(fixed.children[0].children.len(), 1);

        // Second child: original block
        assert!(!fixed.children[1].is_anonymous());
        assert!(fixed.children[1].is_block_level());

        // Third child: anonymous block wrapping text2
        assert!(fixed.children[2].is_anonymous());
        assert!(fixed.children[2].is_block_level());
        assert_eq!(fixed.children[2].children.len(), 1);
    }

    #[test]
    fn test_consecutive_inlines_grouped() {
        let inline1 = BoxNode::new_inline(default_style(), vec![]);
        let inline2 = BoxNode::new_inline(default_style(), vec![]);
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let inline3 = BoxNode::new_inline(default_style(), vec![]);

        let container = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![inline1, inline2, block, inline3],
        );

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        // Should have 3 children: anon[inline1, inline2], block, anon[inline3]
        assert_eq!(fixed.children.len(), 3);

        // First anonymous block contains 2 inlines
        assert!(fixed.children[0].is_anonymous());
        assert_eq!(fixed.children[0].children.len(), 2);

        // Second is the original block
        assert!(!fixed.children[1].is_anonymous());

        // Third anonymous block contains 1 inline
        assert!(fixed.children[2].is_anonymous());
        assert_eq!(fixed.children[2].children.len(), 1);
    }

    #[test]
    fn test_nested_fixup() {
        // Create nested structure that needs fixup at multiple levels
        let inner_text = BoxNode::new_text(default_style(), "Inner".to_string());
        let inner_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let inner_container = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![inner_text, inner_block],
        );

        let outer_text = BoxNode::new_text(default_style(), "Outer".to_string());

        let outer_container = BoxNode::new_block(
            default_style(),
            FormattingContextType::Block,
            vec![outer_text, inner_container],
        );

        let fixed = AnonymousBoxCreator::fixup_tree(outer_container);

        // Outer level should have 2 children (anon block, block)
        assert_eq!(fixed.children.len(), 2);
        assert!(fixed.children[0].is_anonymous());
        assert!(!fixed.children[1].is_anonymous());

        // Inner level should also be fixed
        let inner = &fixed.children[1];
        assert_eq!(inner.children.len(), 2);
        assert!(inner.children[0].is_anonymous());
        assert!(!inner.children[1].is_anonymous());
    }

    #[test]
    fn test_all_inline_content_wraps_text() {
        let inline = BoxNode::new_inline(default_style(), vec![]);
        let text = BoxNode::new_text(default_style(), "Text".to_string());

        let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![inline, text]);

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        // All inline content - text should be wrapped in anonymous inline
        assert_eq!(fixed.children.len(), 2);

        // First is inline (unchanged)
        assert!(!fixed.children[0].is_anonymous());
        assert!(fixed.children[0].is_inline_level());

        // Second is text wrapped in anonymous inline
        assert!(fixed.children[1].is_anonymous());
        assert!(fixed.children[1].is_inline_level());
    }

    #[test]
    fn test_inline_container_wraps_text() {
        let text = BoxNode::new_text(default_style(), "Text".to_string());

        let inline = BoxNode::new_inline(default_style(), vec![text]);

        let fixed = AnonymousBoxCreator::fixup_tree(inline);

        // Text inside inline should be wrapped in anonymous inline
        assert_eq!(fixed.children.len(), 1);
        assert!(fixed.children[0].is_anonymous());
        assert!(fixed.children[0].is_inline_level());
    }

    #[test]
    fn test_has_mixed_content() {
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let inline = BoxNode::new_inline(default_style(), vec![]);

        let mixed = vec![block.clone(), inline.clone()];
        let all_block = vec![block.clone(), block.clone()];
        let all_inline = vec![inline.clone(), inline.clone()];

        assert!(AnonymousBoxCreator::has_mixed_content(&mixed));
        assert!(!AnonymousBoxCreator::has_mixed_content(&all_block));
        assert!(!AnonymousBoxCreator::has_mixed_content(&all_inline));
    }

    #[test]
    fn test_count_anonymous_boxes() {
        let text1 = BoxNode::new_text(default_style(), "Text 1".to_string());
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let text2 = BoxNode::new_text(default_style(), "Text 2".to_string());

        let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text1, block, text2]);

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        // Should have 2 anonymous blocks
        assert_eq!(AnonymousBoxCreator::count_anonymous_boxes(&fixed), 2);
    }

    #[test]
    fn test_text_at_start_and_end() {
        let text1 = BoxNode::new_text(default_style(), "Start".to_string());
        let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
        let text2 = BoxNode::new_text(default_style(), "End".to_string());

        let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text1, block, text2]);

        let fixed = AnonymousBoxCreator::fixup_tree(container);

        assert_eq!(fixed.children.len(), 3);
        assert!(fixed.children[0].is_anonymous());
        assert!(!fixed.children[1].is_anonymous());
        assert!(fixed.children[2].is_anonymous());
    }

    #[test]
    fn test_deeply_nested_structure() {
        // Create: block > block > block > (text, block)
        let text = BoxNode::new_text(default_style(), "Deep".to_string());
        let inner_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

        let level3 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text, inner_block]);

        let level2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level3]);

        let level1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level2]);

        let fixed = AnonymousBoxCreator::fixup_tree(level1);

        // Navigate to level 3 and verify it was fixed
        let level3_fixed = &fixed.children[0].children[0];
        assert_eq!(level3_fixed.children.len(), 2);
        assert!(level3_fixed.children[0].is_anonymous());
    }
}
