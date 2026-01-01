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
//! let fixed_tree = AnonymousBoxCreator::fixup_tree(box_tree)?;
//! ```

use crate::error::{RenderStage, Result};
use crate::render_control::active_deadline;
use crate::style::display::Display;
use crate::style::ComputedStyle;
use crate::tree::box_tree::AnonymousBox;
use crate::tree::box_tree::AnonymousType;
use crate::tree::box_tree::BoxNode;
use crate::tree::box_tree::BoxType;
use crate::tree::box_tree::InlineBox;
use crate::tree::debug::DebugInfo;
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

const ANON_FIXUP_DEADLINE_STRIDE: usize = 256;

enum InlineSplitOutcome {
  Unchanged(BoxNode),
  Split(Vec<BoxNode>),
}

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
  /// use fastrender::{BoxNode, FormattingContextType};
  /// use fastrender::tree::anonymous::AnonymousBoxCreator;
  /// use fastrender::ComputedStyle;
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
  /// let fixed = AnonymousBoxCreator::fixup_tree(container)?;
  /// // Text is now wrapped in anonymous block
  /// assert_eq!(fixed.children.len(), 2);
  /// assert!(fixed.children[0].is_anonymous());
  /// ```
  pub fn fixup_tree(box_node: BoxNode) -> Result<BoxNode> {
    let mut deadline_counter = 0usize;
    Self::fixup_tree_with_deadline(box_node, &mut deadline_counter)
  }

  /// Fixes up a box tree while periodically checking the active render deadline.
  pub fn fixup_tree_with_deadline(
    box_node: BoxNode,
    deadline_counter: &mut usize,
  ) -> Result<BoxNode> {
    let deadline = active_deadline();
    let deadline = deadline.as_ref();
    if let Some(deadline) = deadline {
      deadline.check_periodic(
        deadline_counter,
        ANON_FIXUP_DEADLINE_STRIDE,
        RenderStage::Cascade,
      )?;
    }

    // Bottom-up fixup needs post-order traversal (children first, then parent). The previous
    // implementation used recursion, which can overflow the stack on pathological DOMs with
    // tens of thousands of nodes. Walk the tree iteratively with an explicit stack.
    struct Frame {
      node: *mut BoxNode,
      parent_style: Option<*const Arc<ComputedStyle>>,
      next_child_idx: usize,
    }

    let mut root = box_node;
    let mut stack = vec![Frame {
      node: &mut root as *mut BoxNode,
      parent_style: None,
      next_child_idx: 0,
    }];

    while let Some(frame) = stack.last_mut() {
      // SAFETY: `BoxNode`s aren't moved while they have active frames on the stack. We only
      // replace a node's `children` Vec after all child frames have been popped, so there are
      // no outstanding pointers into that Vec at mutation time.
      let node = unsafe { &mut *frame.node };

      if frame.next_child_idx < node.children.len() {
        let idx = frame.next_child_idx;
        frame.next_child_idx += 1;

        if let Some(deadline) = deadline {
          deadline.check_periodic(
            deadline_counter,
            ANON_FIXUP_DEADLINE_STRIDE,
            RenderStage::Cascade,
          )?;
        }

        let child_ptr = &mut node.children[idx] as *mut BoxNode;
        stack.push(Frame {
          node: child_ptr,
          parent_style: Some(&node.style as *const Arc<ComputedStyle>),
          next_child_idx: 0,
        });
        continue;
      }

      let parent_style = match frame.parent_style {
        Some(style) => unsafe { &*style },
        None => &node.style,
      };
      let children = std::mem::take(&mut node.children);
      node.children = Self::fixup_children(children, &node.box_type, parent_style);
      stack.pop();
    }

    Ok(root)
  }

  /// Fixes up children based on parent box type
  ///
  /// Dispatches to appropriate fixup method based on whether parent is
  /// a block container or inline container.
  fn fixup_children(
    children: Vec<BoxNode>,
    parent_type: &BoxType,
    parent_style: &Arc<ComputedStyle>,
  ) -> Vec<BoxNode> {
    if children.is_empty() {
      return children;
    }

    match parent_type {
      // Block containers need block fixup
      BoxType::Block(_) => Self::fixup_block_children(children, parent_style.as_ref()),

      // Inline containers need inline fixup
      BoxType::Inline(_) => Self::fixup_inline_children(children, parent_style),

      // Anonymous block containers also need block fixup
      BoxType::Anonymous(anon)
        if matches!(
          anon.anonymous_type,
          AnonymousType::Block | AnonymousType::TableCell
        ) =>
      {
        Self::fixup_block_children(children, parent_style.as_ref())
      }

      // Anonymous inline containers need inline fixup
      BoxType::Anonymous(anon) if matches!(anon.anonymous_type, AnonymousType::Inline) => {
        Self::fixup_inline_children(children, parent_style)
      }

      // Text boxes, replaced elements, and others don't need fixup
      _ => children,
    }
  }

  /// Determines whether a child should participate as inline-level content for fixup.
  ///
  /// Replaced elements with inline display behave as inline-level boxes even though
  /// `BoxType::is_inline_level` returns false.
  fn is_inline_level_child(child: &BoxNode) -> bool {
    match &child.box_type {
      BoxType::Replaced(_) => child.style.display.is_inline_level(),
      _ => child.is_inline_level(),
    }
  }

  /// Determines whether a child should participate as block-level content for fixup.
  ///
  /// Replaced elements with inline display are treated as inline-level and shouldn't
  /// be counted as blocks when deciding anonymous box insertion.
  fn is_block_level_child(child: &BoxNode) -> bool {
    match &child.box_type {
      BoxType::Replaced(_) => !child.style.display.is_inline_level(),
      _ => child.is_block_level(),
    }
  }

  /// Returns true if an inline box (without its own formatting context) contains
  /// any block-level descendants.
  fn inline_contains_block_descendants(node: &BoxNode) -> bool {
    if node.formatting_context().is_some() {
      return false;
    }

    let mut stack: Vec<&BoxNode> = Vec::new();
    stack.push(node);
    while let Some(current) = stack.pop() {
      for child in &current.children {
        if Self::is_block_level_child(child) {
          return true;
        }
        if Self::is_inline_level_child(child)
          && child.formatting_context().is_none()
          && !child.children.is_empty()
        {
          stack.push(child);
        }
      }
    }

    false
  }

  /// Splits an inline box around any block-level descendants, returning either the original
  /// box (when no split is required) or a list of boxes that should replace the original
  /// inline in its parent.
  ///
  /// Inline fragments preserve the original inline's style/debug info; block descendants
  /// are lifted into the returned list so callers can place them directly in the block
  /// formatting context.
  fn split_inline_with_blocks(inline: BoxNode) -> InlineSplitOutcome {
    // Fast path: nothing under this inline can violate the "no blocks inside inlines" rule.
    // This avoids allocating replacement vectors for the overwhelmingly common case.
    if inline.formatting_context().is_some()
      || inline.children.is_empty()
      || !Self::inline_contains_block_descendants(&inline)
    {
      return InlineSplitOutcome::Unchanged(inline);
    }

    struct Frame {
      node: BoxNode,
      children: std::vec::IntoIter<BoxNode>,
      inline_run: Vec<BoxNode>,
      out: Vec<BoxNode>,
      had_block: bool,
    }

    impl Frame {
      fn new(mut node: BoxNode) -> Self {
        let children_vec = std::mem::take(&mut node.children);
        let inline_run = Vec::with_capacity(children_vec.len());
        let children = children_vec.into_iter();
        Self {
          node,
          children,
          inline_run,
          out: Vec::new(),
          had_block: false,
        }
      }

      fn flush_inline_run(&mut self) {
        if self.inline_run.is_empty() {
          return;
        }
        let fragment = AnonymousBoxCreator::clone_inline_fragment(
          &self.node.style,
          self.node.starting_style.clone(),
          &self.node.box_type,
          self.node.debug_info.as_ref(),
          self.node.styled_node_id,
          std::mem::take(&mut self.inline_run),
        );
        self.out.push(fragment);
      }

      fn push_piece(&mut self, piece: BoxNode) {
        if AnonymousBoxCreator::is_block_level_child(&piece) {
          self.had_block = true;
          self.flush_inline_run();
          self.out.push(piece);
        } else {
          self.inline_run.push(piece);
        }
      }
    }

    let mut stack: Vec<Frame> = Vec::new();
    stack.push(Frame::new(inline));

    loop {
      let Some(top) = stack.last_mut() else {
        unreachable!("split stack always returns from within the loop");
      };

      if let Some(child) = top.children.next() {
        if Self::is_block_level_child(&child) {
          top.had_block = true;
          top.flush_inline_run();
          top.out.push(child);
          continue;
        }

        if Self::is_inline_level_child(&child)
          && child.formatting_context().is_none()
          && !child.children.is_empty()
        {
          stack.push(Frame::new(child));
          continue;
        }

        top.inline_run.push(child);
        continue;
      }

      let mut finished = stack.pop().expect("frame popped");
      if finished.had_block {
        finished.flush_inline_run();
        let pieces = finished.out;
        if let Some(parent) = stack.last_mut() {
          for piece in pieces {
            parent.push_piece(piece);
          }
          continue;
        }
        return InlineSplitOutcome::Split(pieces);
      }

      finished.node.children = finished.inline_run;
      let node = finished.node;
      if let Some(parent) = stack.last_mut() {
        parent.inline_run.push(node);
        continue;
      }
      return InlineSplitOutcome::Unchanged(node);
    }
  }

  /// Splits inline-level children that contain block-level descendants so that block
  /// boxes participate directly in the parent's block formatting context.
  fn split_inline_children_with_block_descendants(mut children: Vec<BoxNode>) -> Vec<BoxNode> {
    // Fast path: if no inline-level child contains block descendants, return the original Vec to
    // preserve its allocation.
    let Some(first_split_idx) = children.iter().position(|child| {
      Self::is_inline_level_child(child)
        && child.formatting_context().is_none()
        && Self::inline_contains_block_descendants(child)
    }) else {
      return children;
    };

    let mut result = Vec::with_capacity(children.len());
    result.extend(children.drain(..first_split_idx));

    let mut iter = children.into_iter();
    let first_child = iter
      .next()
      .expect("first_split_idx should point at an in-bounds child");
    // We already proved the first child needs splitting.
    match Self::split_inline_with_blocks(first_child) {
      InlineSplitOutcome::Unchanged(node) => result.push(node),
      InlineSplitOutcome::Split(pieces) => result.extend(pieces),
    }

    for child in iter {
      if Self::is_inline_level_child(&child)
        && child.formatting_context().is_none()
        && Self::inline_contains_block_descendants(&child)
      {
        match Self::split_inline_with_blocks(child) {
          InlineSplitOutcome::Unchanged(node) => result.push(node),
          InlineSplitOutcome::Split(pieces) => result.extend(pieces),
        }
      } else {
        result.push(child);
      }
    }

    result
  }

  /// Clones an inline box fragment preserving the inline/anonymous inline identity and debug info.
  fn clone_inline_fragment(
    style: &Arc<ComputedStyle>,
    starting_style: Option<Arc<ComputedStyle>>,
    box_type: &BoxType,
    debug_info: Option<&DebugInfo>,
    styled_node_id: Option<usize>,
    children: Vec<BoxNode>,
  ) -> BoxNode {
    let fragment = match box_type {
      BoxType::Inline(inline) => BoxNode {
        style: style.clone(),
        starting_style: starting_style.clone(),
        box_type: BoxType::Inline(InlineBox {
          formatting_context: inline.formatting_context,
        }),
        children,
        id: 0,
        debug_info: debug_info.cloned(),
        styled_node_id,
        first_line_style: None,
        first_letter_style: None,
      },
      BoxType::Anonymous(anon) if matches!(anon.anonymous_type, AnonymousType::Inline) => BoxNode {
        style: style.clone(),
        starting_style: starting_style.clone(),
        box_type: BoxType::Anonymous(anon.clone()),
        children,
        id: 0,
        debug_info: debug_info.cloned(),
        styled_node_id,
        first_line_style: None,
        first_letter_style: None,
      },
      _ => {
        let mut node = BoxNode::new_inline(style.clone(), children);
        node.debug_info = debug_info.cloned();
        node.styled_node_id = styled_node_id;
        node.starting_style = starting_style;
        node
      }
    };

    fragment
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
  fn fixup_block_children(children: Vec<BoxNode>, parent_style: &ComputedStyle) -> Vec<BoxNode> {
    // First, split any inline boxes that illegally contain block-level descendants
    // so the block descendants participate directly in the block formatting context.
    // Fast path: if there are no inline-level children, there can't be any illegal
    // "inline contains block" situations either.
    let mut children = children;
    if !children.iter().any(Self::is_inline_level_child) {
      return children;
    }
    children = Self::split_inline_children_with_block_descendants(children);

    // Determine what kind of content we have after splitting
    let mut has_block = false;
    let mut has_inline = false;
    for child in &children {
      has_block |= Self::is_block_level_child(child);
      has_inline |= Self::is_inline_level_child(child);
      if has_block && has_inline {
        break;
      }
    }

    if has_block && has_inline {
      // Mixed content - wrap inline runs in anonymous blocks
      Self::wrap_inline_runs_in_anonymous_blocks(children, parent_style)
    } else if !has_block && has_inline {
      // All inline content - wrap bare text in anonymous inlines
      // (This maintains proper inline structure)
      Self::wrap_bare_text_in_anonymous_inline(children, parent_style)
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
  fn fixup_inline_children(
    children: Vec<BoxNode>,
    parent_style: &Arc<ComputedStyle>,
  ) -> Vec<BoxNode> {
    // Wrap bare text nodes in anonymous inline boxes while reusing the existing Vec allocation.
    let mut children = children;
    let mut inline_style: Option<Arc<ComputedStyle>> = None;

    for child in children.iter_mut() {
      if !child.is_text() {
        continue;
      }

      let style = inline_style.get_or_insert_with(|| {
        if parent_style.display == Display::Inline {
          parent_style.clone()
        } else {
          let mut style = (**parent_style).clone();
          style.display = Display::Inline;
          Arc::new(style)
        }
      });

      Self::wrap_text_in_anonymous_inline_in_place(child, style.clone());
    }

    children
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
  fn wrap_inline_runs_in_anonymous_blocks(
    children: Vec<BoxNode>,
    parent_style: &ComputedStyle,
  ) -> Vec<BoxNode> {
    let mut result = Vec::with_capacity(children.len());
    let mut inline_run: Vec<BoxNode> = Vec::new();
    let mut anonymous_block_style: Option<Arc<ComputedStyle>> = None;

    for child in children {
      if Self::is_inline_level_child(&child) {
        // Accumulate inline boxes
        inline_run.push(child);
      } else {
        // Block box encountered - flush any inline run
        if !inline_run.is_empty() {
          let style = anonymous_block_style.get_or_insert_with(|| {
            let mut style = inherited_style(parent_style);
            style.display = Display::Block;
            Arc::new(style)
          });
          let anon_block =
            Self::create_anonymous_block(style.clone(), std::mem::take(&mut inline_run));
          result.push(anon_block);
        }
        result.push(child);
      }
    }

    // Flush remaining inline run at end
    if !inline_run.is_empty() {
      let style = anonymous_block_style.get_or_insert_with(|| {
        let mut style = inherited_style(parent_style);
        style.display = Display::Block;
        Arc::new(style)
      });
      let anon_block = Self::create_anonymous_block(style.clone(), std::mem::take(&mut inline_run));
      result.push(anon_block);
    }

    result
  }

  /// Wraps bare text nodes in anonymous inline boxes
  ///
  /// CSS 2.1 Section 9.2.2.1: Text directly inside a block container
  /// that isn't inside an inline element gets an anonymous inline box.
  fn wrap_bare_text_in_anonymous_inline(
    children: Vec<BoxNode>,
    parent_style: &ComputedStyle,
  ) -> Vec<BoxNode> {
    let mut inherited_inline_style: Option<Arc<ComputedStyle>> = None;
    let mut children = children;
    for child in children.iter_mut() {
      if !child.is_text() {
        continue;
      }
      // Wrap text in anonymous inline.
      let style = inherited_inline_style.get_or_insert_with(|| {
        let mut inherited = inherited_style(parent_style);
        inherited.display = Display::Inline;
        Arc::new(inherited)
      });
      Self::wrap_text_in_anonymous_inline_in_place(child, style.clone());
    }

    children
  }

  fn wrap_text_in_anonymous_inline_in_place(node: &mut BoxNode, wrapper_style: Arc<ComputedStyle>) {
    debug_assert!(node.is_text());

    let wrapper_box_type = BoxType::Anonymous(AnonymousBox {
      anonymous_type: AnonymousType::Inline,
    });

    let text_node = BoxNode {
      style: std::mem::replace(&mut node.style, wrapper_style),
      starting_style: node.starting_style.take(),
      box_type: std::mem::replace(&mut node.box_type, wrapper_box_type),
      children: std::mem::take(&mut node.children),
      id: std::mem::replace(&mut node.id, 0),
      debug_info: node.debug_info.take(),
      styled_node_id: node.styled_node_id.take(),
      first_line_style: node.first_line_style.take(),
      first_letter_style: node.first_letter_style.take(),
    };

    node.children = vec![text_node];
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
    let style = if style.display == Display::Block {
      style
    } else {
      let mut style = (*style).clone();
      style.display = Display::Block;
      Arc::new(style)
    };
    BoxNode {
      style,
      starting_style: None,
      box_type: BoxType::Anonymous(AnonymousBox {
        anonymous_type: AnonymousType::Block,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
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
    let style = if style.display == Display::Inline {
      style
    } else {
      let mut style = (*style).clone();
      style.display = Display::Inline;
      Arc::new(style)
    };
    BoxNode {
      style,
      starting_style: None,
      box_type: BoxType::Anonymous(AnonymousBox {
        anonymous_type: AnonymousType::Inline,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates an anonymous table row box
  ///
  /// Used when table cells appear outside of table rows.
  pub fn create_anonymous_table_row(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> BoxNode {
    BoxNode {
      style,
      starting_style: None,
      box_type: BoxType::Anonymous(AnonymousBox {
        anonymous_type: AnonymousType::TableRow,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Creates an anonymous table cell box
  ///
  /// Used when non-table content appears inside table rows.
  pub fn create_anonymous_table_cell(style: Arc<ComputedStyle>, children: Vec<BoxNode>) -> BoxNode {
    BoxNode {
      style,
      starting_style: None,
      box_type: BoxType::Anonymous(AnonymousBox {
        anonymous_type: AnonymousType::TableCell,
      }),
      children,
      id: 0,
      debug_info: None,
      styled_node_id: None,
      first_line_style: None,
      first_letter_style: None,
    }
  }

  /// Checks if a list of children contains mixed block/inline content
  ///
  /// Returns true if children contain both block-level and inline-level boxes.
  pub fn has_mixed_content(children: &[BoxNode]) -> bool {
    let has_block = children.iter().any(Self::is_block_level_child);
    let has_inline = children.iter().any(Self::is_inline_level_child);
    has_block && has_inline
  }

  /// Checks if all children are block-level
  pub fn all_block_level(children: &[BoxNode]) -> bool {
    children.iter().all(Self::is_block_level_child)
  }

  /// Checks if all children are inline-level
  pub fn all_inline_level(children: &[BoxNode]) -> bool {
    children.iter().all(Self::is_inline_level_child)
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

/// Build a style that inherits inheritable properties from the parent while leaving
/// non-inherited properties at their initial values.
pub(crate) fn inherited_style(parent: &ComputedStyle) -> ComputedStyle {
  let mut style = ComputedStyle::default();
  // Typography / text
  style.font_family = parent.font_family.clone();
  style.font_size = parent.font_size;
  style.root_font_size = parent.root_font_size;
  style.font_weight = parent.font_weight;
  style.font_style = parent.font_style;
  style.font_variant = parent.font_variant;
  style.font_variant_caps = parent.font_variant_caps;
  style.font_variant_alternates = parent.font_variant_alternates.clone();
  style.font_variant_numeric = parent.font_variant_numeric;
  style.font_variant_east_asian = parent.font_variant_east_asian;
  style.font_variant_ligatures = parent.font_variant_ligatures;
  style.font_variant_position = parent.font_variant_position;
  style.font_size_adjust = parent.font_size_adjust;
  style.font_synthesis = parent.font_synthesis;
  style.font_feature_settings = parent.font_feature_settings.clone();
  style.font_optical_sizing = parent.font_optical_sizing;
  style.font_variation_settings = parent.font_variation_settings.clone();
  style.font_language_override = parent.font_language_override.clone();
  style.font_variant_emoji = parent.font_variant_emoji;
  style.font_stretch = parent.font_stretch;
  style.font_kerning = parent.font_kerning;
  style.line_height = parent.line_height.clone();
  style.direction = parent.direction;
  style.text_align = parent.text_align;
  style.text_align_last = parent.text_align_last;
  style.text_justify = parent.text_justify;
  style.text_indent = parent.text_indent;
  style.text_decoration_skip_ink = parent.text_decoration_skip_ink;
  style.text_underline_offset = parent.text_underline_offset;
  style.text_underline_position = parent.text_underline_position;
  style.text_emphasis_style = parent.text_emphasis_style.clone();
  style.text_emphasis_color = parent.text_emphasis_color;
  style.text_emphasis_position = parent.text_emphasis_position;
  style.text_transform = parent.text_transform;
  style.text_combine_upright = parent.text_combine_upright;
  style.writing_mode = parent.writing_mode;
  style.letter_spacing = parent.letter_spacing;
  style.word_spacing = parent.word_spacing;
  style.justify_items = parent.justify_items;
  style.visibility = parent.visibility;
  style.white_space = parent.white_space;
  style.line_break = parent.line_break;
  style.tab_size = parent.tab_size;
  style.caption_side = parent.caption_side;
  style.empty_cells = parent.empty_cells;
  style.hyphens = parent.hyphens;
  style.word_break = parent.word_break;
  style.overflow_wrap = parent.overflow_wrap;
  style.language = parent.language.clone();
  style.list_style_type = parent.list_style_type.clone();
  style.list_style_position = parent.list_style_position;
  style.list_style_image = parent.list_style_image.clone();
  style.quotes = parent.quotes.clone();
  style.cursor = parent.cursor;
  style.cursor_images = parent.cursor_images.clone();
  style.color = parent.color;
  style.custom_property_registry = parent.custom_property_registry.clone();
  style.custom_properties = parent.custom_properties.clone();
  style
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::style::display::FormattingContextType;
  use crate::tree::table_fixup::TableStructureFixer;

  fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
  }

  fn fixup_tree(node: BoxNode) -> BoxNode {
    super::AnonymousBoxCreator::fixup_tree(node).expect("anonymous fixup")
  }

  fn drop_box_node_iterative(root: BoxNode) {
    let mut stack = vec![root];
    while let Some(mut node) = stack.pop() {
      stack.append(&mut node.children);
    }
  }

  #[test]
  fn test_fixup_deep_tree_stack_safe() {
    // This used to be recursive (both anonymous fixup and table fixup). Deep trees triggered
    // stack overflows in real-world pages; keep this test large enough to fail with recursion.
    let style = default_style();
    let mut node = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    for _ in 0..30_000 {
      node = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![node]);
    }

    let fixed = AnonymousBoxCreator::fixup_tree(node).expect("anonymous fixup");
    let fixed = TableStructureFixer::fixup_tree_internals(fixed).expect("table fixup");
    assert!(fixed.is_block_level());
    // Dropping a deeply nested `BoxNode` is itself recursive (via field drop order) and can
    // overflow the stack. The fixup passes should be stack-safe regardless, so drop the result
    // iteratively here.
    drop_box_node_iterative(fixed);
  }

  #[test]
  fn test_split_inline_with_blocks_stack_safe() {
    // Deeply nested inlines containing a block used to recurse both in the "contains block"
    // detection and in the splitting logic.
    let style = default_style();
    let block = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![]);
    let mut inline = BoxNode::new_inline(style.clone(), vec![block]);
    for _ in 0..20_000 {
      inline = BoxNode::new_inline(style.clone(), vec![inline]);
    }

    let root = BoxNode::new_block(style.clone(), FormattingContextType::Block, vec![inline]);
    let fixed = fixup_tree(root);
    assert_eq!(fixed.children.len(), 1);
    assert!(fixed.children[0].is_block_level());
  }

  #[test]
  fn test_empty_container_no_crash() {
    let empty_block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let fixed = fixup_tree(empty_block);
    assert_eq!(fixed.children.len(), 0);
  }

  #[test]
  fn test_all_block_children_no_change() {
    let child1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let child2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);

    let container = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![child1, child2],
    );

    let fixed = fixup_tree(container);

    // Should still have 2 children, no wrapping
    assert_eq!(fixed.children.len(), 2);
    assert!(!fixed.children[0].is_anonymous());
    assert!(!fixed.children[1].is_anonymous());
  }

  #[test]
  fn test_single_text_wrapped() {
    let text = BoxNode::new_text(default_style(), "Hello".to_string());

    let container = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![text]);

    let fixed = fixup_tree(container);

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

    let container = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![text1, block, text2],
    );

    let fixed = fixup_tree(container);

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

    let fixed = fixup_tree(container);

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

    let fixed = fixup_tree(outer_container);

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

    let container = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![inline, text],
    );

    let fixed = fixup_tree(container);

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

    let fixed = fixup_tree(inline);

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

    let container = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![text1, block, text2],
    );

    let fixed = fixup_tree(container);

    // Should have 2 anonymous blocks
    assert_eq!(AnonymousBoxCreator::count_anonymous_boxes(&fixed), 2);
  }

  #[test]
  fn test_text_at_start_and_end() {
    let text1 = BoxNode::new_text(default_style(), "Start".to_string());
    let block = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![]);
    let text2 = BoxNode::new_text(default_style(), "End".to_string());

    let container = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![text1, block, text2],
    );

    let fixed = fixup_tree(container);

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

    let level3 = BoxNode::new_block(
      default_style(),
      FormattingContextType::Block,
      vec![text, inner_block],
    );

    let level2 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level3]);

    let level1 = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![level2]);

    let fixed = fixup_tree(level1);

    // Navigate to level 3 and verify it was fixed
    let level3_fixed = &fixed.children[0].children[0];
    assert_eq!(level3_fixed.children.len(), 2);
    assert!(level3_fixed.children[0].is_anonymous());
  }
}
