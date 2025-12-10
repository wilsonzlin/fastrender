//! Stacking Context Tree
//!
//! This module implements CSS stacking contexts for determining paint order.
//! Stacking contexts control how elements with z-index, opacity, transforms,
//! and other properties are layered during painting.
//!
//! # CSS Specification
//!
//! - CSS 2.1 Appendix E: Elaborate description of Stacking Contexts
//!   <https://www.w3.org/TR/CSS21/zindex.html>
//! - CSS 2.1 Section 9.9: Layered presentation
//!   <https://www.w3.org/TR/CSS21/visuren.html#layered-presentation>
//!
//! # The 7-Layer Paint Order Algorithm
//!
//! Within each stacking context, elements are painted in this order:
//!
//! 1. Background and borders of the stacking context root
//! 2. Child stacking contexts with negative z-index (most negative first)
//! 3. In-flow, non-inline-level descendants (block boxes in tree order)
//! 4. Non-positioned floats (tree order)
//! 5. In-flow, inline-level descendants (inline boxes and text in tree order)
//! 6. Positioned descendants with z-index 0 or auto (tree order)
//! 7. Child stacking contexts with positive z-index (least positive first)
//!
//! # Stacking Context Creation
//!
//! An element creates a stacking context if it satisfies ANY of these conditions:
//!
//! 1. Root element (`<html>`)
//! 2. Positioned element with z-index ≠ auto (relative/absolute/fixed/sticky + z-index: `<integer>`)
//! 3. Fixed or sticky positioning (even without z-index)
//! 4. Opacity < 1
//! 5. Any transform (except none)
//! 6. Filter property (except none)
//! 7. Clip-path property (except none)
//! 8. Mask properties
//! 9. Mix-blend-mode (except normal)
//! 10. Isolation: isolate
//! 11. Perspective property (except none)
//! 12. Backdrop-filter property (except none)
//! 13. Containment properties (contain: layout|paint|strict|content)
//! 14. Flex items with z-index (child of flex container with z-index)
//! 15. Grid items with z-index (child of grid container with z-index)
//! 16. Will-change set to property that creates stacking context
//! 17. Container type (size or inline-size)
//! 18. Top layer elements (fullscreen, popover, dialog)
//!
//! # Usage
//!
//! ```ignore
//! use fastrender::paint::stacking::{StackingContext, build_stacking_tree};
//! use fastrender::tree::FragmentTree;
//!
//! let fragment_tree = /* ... */;
//! let stacking_tree = build_stacking_tree(&fragment_tree.root, None, true);
//! ```

use crate::geometry::Rect;
use crate::style::display::Display;
use crate::style::position::Position;
use crate::style::types::Overflow;
use crate::style::ComputedStyle;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use std::cmp::Ordering;
use std::sync::Arc;

/// A reference to a fragment with associated style information
///
/// Since FragmentNode doesn't carry style information directly,
/// we use this wrapper to associate fragments with their computed styles
/// for stacking context operations.
#[derive(Debug, Clone)]
pub struct StyledFragmentRef<'a> {
    /// The fragment node
    pub fragment: &'a FragmentNode,

    /// The computed style for this fragment (if available)
    pub style: Option<Arc<ComputedStyle>>,

    /// Tree order index (for sorting tie-breaking)
    pub tree_order: usize,
}

impl<'a> StyledFragmentRef<'a> {
    /// Creates a new styled fragment reference
    pub fn new(fragment: &'a FragmentNode, style: Option<Arc<ComputedStyle>>, tree_order: usize) -> Self {
        Self {
            fragment,
            style,
            tree_order,
        }
    }
}

/// Reasons why a stacking context was created
///
/// Used for debugging and understanding the stacking context tree structure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackingContextReason {
    /// Root element of the document
    Root,

    /// Positioned element (relative/absolute/fixed/sticky) with z-index != auto
    PositionedWithZIndex,

    /// Fixed positioning (always creates stacking context)
    FixedPositioning,

    /// Sticky positioning (always creates stacking context)
    StickyPositioning,

    /// Opacity < 1.0
    Opacity,

    /// Has CSS transform
    Transform,

    /// Has CSS filter
    Filter,

    /// Has CSS clip-path
    ClipPath,

    /// Has CSS mask
    Mask,

    /// mix-blend-mode != normal
    MixBlendMode,

    /// isolation: isolate
    Isolation,

    /// Has CSS perspective
    Perspective,

    /// Has backdrop-filter
    BackdropFilter,

    /// CSS containment (layout, paint, etc.)
    Containment,

    /// Flex item with z-index
    FlexItemWithZIndex,

    /// Grid item with z-index
    GridItemWithZIndex,

    /// will-change triggers stacking context
    WillChange,

    /// container-type creates stacking context
    ContainerType,

    /// Top layer element (fullscreen, popover, dialog)
    TopLayer,

    /// Overflow hidden/scroll/auto (in some contexts)
    OverflowClip,
}

/// A stacking context in the stacking context tree
///
/// Represents a layer in the paint order hierarchy. Child stacking contexts
/// are sorted by z-index, and descendants within a stacking context are
/// organized into paint layers.
///
/// # Example
///
/// ```ignore
/// let sc = StackingContext::new(0);
/// assert_eq!(sc.z_index, 0);
/// assert!(sc.children.is_empty());
/// ```
#[derive(Debug, Clone)]
pub struct StackingContext {
    /// Z-index value for this stacking context
    ///
    /// - For root: 0
    /// - For positioned elements with z-index: the z-index value
    /// - For auto-created contexts (opacity, transform): 0
    pub z_index: i32,

    /// Child stacking contexts (will be sorted by z-index for painting)
    pub children: Vec<StackingContext>,

    /// Fragments that belong directly to this stacking context
    /// (organized by paint layer)
    pub fragments: Vec<FragmentNode>,

    /// Layer 3: In-flow block-level descendants (tree order)
    pub layer3_blocks: Vec<FragmentNode>,

    /// Layer 4: Non-positioned floats (tree order)
    pub layer4_floats: Vec<FragmentNode>,

    /// Layer 5: In-flow inline-level descendants (tree order)
    pub layer5_inlines: Vec<FragmentNode>,

    /// Layer 6: Positioned descendants with z-index 0 or auto (tree order)
    pub layer6_positioned: Vec<FragmentNode>,

    /// Bounds of this stacking context (for culling and hit testing)
    pub bounds: Rect,

    /// Why this stacking context was created (for debugging)
    pub reason: StackingContextReason,

    /// Tree order index for stable sorting
    pub tree_order: usize,
}

impl StackingContext {
    /// Creates a new stacking context with the given z-index
    pub fn new(z_index: i32) -> Self {
        Self {
            z_index,
            children: Vec::new(),
            fragments: Vec::new(),
            layer3_blocks: Vec::new(),
            layer4_floats: Vec::new(),
            layer5_inlines: Vec::new(),
            layer6_positioned: Vec::new(),
            bounds: Rect::from_xywh(0.0, 0.0, 0.0, 0.0),
            reason: StackingContextReason::Root,
            tree_order: 0,
        }
    }

    /// Creates a new stacking context with reason
    pub fn with_reason(z_index: i32, reason: StackingContextReason, tree_order: usize) -> Self {
        Self {
            z_index,
            children: Vec::new(),
            fragments: Vec::new(),
            layer3_blocks: Vec::new(),
            layer4_floats: Vec::new(),
            layer5_inlines: Vec::new(),
            layer6_positioned: Vec::new(),
            bounds: Rect::from_xywh(0.0, 0.0, 0.0, 0.0),
            reason,
            tree_order,
        }
    }

    /// Creates a root stacking context
    pub fn root() -> Self {
        Self::with_reason(0, StackingContextReason::Root, 0)
    }

    /// Adds a child stacking context
    pub fn add_child(&mut self, child: StackingContext) {
        self.children.push(child);
    }

    /// Adds a fragment to the appropriate layer based on its properties
    pub fn add_fragment_to_layer(&mut self, fragment: FragmentNode, style: Option<&ComputedStyle>) {
        if let Some(style) = style {
            // Determine which layer this fragment belongs to
            if is_positioned(style) && !creates_stacking_context(style, None, false) {
                // Layer 6: Positioned with z-index 0 or auto
                self.layer6_positioned.push(fragment);
            } else if is_float(style) {
                // Layer 4: Floats
                self.layer4_floats.push(fragment);
            } else if is_inline_level(style, &fragment) {
                // Layer 5: Inline-level
                self.layer5_inlines.push(fragment);
            } else {
                // Layer 3: Block-level
                self.layer3_blocks.push(fragment);
            }
        } else {
            // No style info - classify based on fragment content
            match &fragment.content {
                FragmentContent::Text { .. } | FragmentContent::Inline { .. } => {
                    self.layer5_inlines.push(fragment);
                }
                FragmentContent::Line { .. } => {
                    self.layer5_inlines.push(fragment);
                }
                _ => {
                    self.layer3_blocks.push(fragment);
                }
            }
        }
    }

    /// Returns child stacking contexts with negative z-index, sorted (most negative first)
    pub fn negative_z_children(&self) -> Vec<&StackingContext> {
        let mut negative: Vec<_> = self.children.iter().filter(|c| c.z_index < 0).collect();
        negative.sort_by(|a, b| match a.z_index.cmp(&b.z_index) {
            Ordering::Equal => a.tree_order.cmp(&b.tree_order),
            other => other,
        });
        negative
    }

    /// Returns child stacking contexts with zero z-index, sorted by tree order
    pub fn zero_z_children(&self) -> Vec<&StackingContext> {
        let mut zero: Vec<_> = self.children.iter().filter(|c| c.z_index == 0).collect();
        zero.sort_by_key(|c| c.tree_order);
        zero
    }

    /// Returns child stacking contexts with positive z-index, sorted (least positive first)
    pub fn positive_z_children(&self) -> Vec<&StackingContext> {
        let mut positive: Vec<_> = self.children.iter().filter(|c| c.z_index > 0).collect();
        positive.sort_by(|a, b| match a.z_index.cmp(&b.z_index) {
            Ordering::Equal => a.tree_order.cmp(&b.tree_order),
            other => other,
        });
        positive
    }

    /// Sorts all child stacking contexts by z-index (for paint order)
    pub fn sort_children(&mut self) {
        self.children.sort_by(|a, b| match a.z_index.cmp(&b.z_index) {
            Ordering::Equal => a.tree_order.cmp(&b.tree_order),
            other => other,
        });

        // Recursively sort grandchildren
        for child in &mut self.children {
            child.sort_children();
        }
    }

    /// Computes bounds from all fragments in this context
    pub fn compute_bounds(&mut self) {
        let mut bounds = Rect::from_xywh(0.0, 0.0, 0.0, 0.0);

        // Union all fragment bounds from all layers
        for frag in &self.fragments {
            bounds = bounds.union(frag.bounds);
        }
        for frag in &self.layer3_blocks {
            bounds = bounds.union(frag.bounds);
        }
        for frag in &self.layer4_floats {
            bounds = bounds.union(frag.bounds);
        }
        for frag in &self.layer5_inlines {
            bounds = bounds.union(frag.bounds);
        }
        for frag in &self.layer6_positioned {
            bounds = bounds.union(frag.bounds);
        }

        // Union child stacking context bounds
        for child in &self.children {
            bounds = bounds.union(child.bounds);
        }

        self.bounds = bounds;
    }

    /// Returns total fragment count across all layers
    pub fn fragment_count(&self) -> usize {
        self.fragments.len()
            + self.layer3_blocks.len()
            + self.layer4_floats.len()
            + self.layer5_inlines.len()
            + self.layer6_positioned.len()
    }

    /// Returns total count including children (recursive)
    pub fn total_fragment_count(&self) -> usize {
        let mut count = self.fragment_count();
        for child in &self.children {
            count += child.total_fragment_count();
        }
        count
    }
}

/// Checks if an element creates a stacking context
///
/// Implements the comprehensive check for all 19 conditions that create
/// stacking contexts in CSS.
///
/// # Arguments
///
/// * `style` - The computed style for the element
/// * `parent_style` - The parent element's computed style (for flex/grid item checks)
/// * `is_root` - Whether this is the root element
///
/// # Returns
///
/// `true` if the element creates a stacking context
///
/// # Example
///
/// ```ignore
/// use fastrender::paint::stacking::creates_stacking_context;
/// use fastrender::ComputedStyle;
///
/// let mut style = ComputedStyle::default();
/// style.opacity = 0.5;
///
/// assert!(creates_stacking_context(&style, None, false));
/// ```
pub fn creates_stacking_context(style: &ComputedStyle, parent_style: Option<&ComputedStyle>, is_root: bool) -> bool {
    // 1. Root element always creates stacking context
    if is_root {
        return true;
    }

    // 2. Positioned element with z-index != auto
    if is_positioned(style) && style.z_index.is_some() {
        return true;
    }

    // 3. Fixed positioning always creates stacking context
    if matches!(style.position, Position::Fixed) {
        return true;
    }

    // 4. Sticky positioning always creates stacking context
    if matches!(style.position, Position::Sticky) {
        return true;
    }

    // 5. Opacity < 1.0
    if style.opacity < 1.0 {
        return true;
    }

    // 6. Has CSS transform (transform list is non-empty)
    if !style.transform.is_empty() {
        return true;
    }

    // 6b. Has CSS filter (filter list is non-empty)
    if !style.filter.is_empty() {
        return true;
    }

    // 6c. Backdrop filter
    if !style.backdrop_filter.is_empty() {
        return true;
    }

    // 7. Mix-blend-mode or isolation
    if !matches!(style.mix_blend_mode, crate::style::types::MixBlendMode::Normal) {
        return true;
    }
    if matches!(style.isolation, crate::style::types::Isolation::Isolate) {
        return true;
    }

    // 7. Overflow hidden/scroll/auto with visible overflow on the other axis
    // This creates a stacking context in some browsers
    // For simplicity, we create stacking context for any non-visible overflow
    if matches!(
        style.overflow_x,
        Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
    ) || matches!(
        style.overflow_y,
        Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
    ) {
        // Only if positioned, this creates a stacking context
        if is_positioned(style) {
            return true;
        }
    }

    // 14/15. Flex/Grid items with z-index
    // If parent is flex/grid container and this element has z-index != 0
    if let Some(parent) = parent_style {
        let parent_is_flex_or_grid = matches!(
            parent.display,
            Display::Flex | Display::InlineFlex | Display::Grid | Display::InlineGrid
        );
        if parent_is_flex_or_grid && style.z_index.is_some() {
            return true;
        }
    }

    // Note: The following conditions are not currently tracked in ComputedStyle
    // but would create stacking contexts if implemented:
    // - filter property (except none)
    // - clip-path property (except none)
    // - mask properties
    // - mix-blend-mode (except normal)
    // - isolation: isolate
    // - perspective property (except none)
    // - backdrop-filter property (except none)
    // - contain: layout/paint/strict/content
    // - will-change for stacking-context-creating properties
    // - container-type: size/inline-size
    // - top layer elements (fullscreen, popover, dialog)

    false
}

/// Gets the reason why an element creates a stacking context
///
/// Returns `None` if the element doesn't create a stacking context.
pub fn get_stacking_context_reason(
    style: &ComputedStyle,
    parent_style: Option<&ComputedStyle>,
    is_root: bool,
) -> Option<StackingContextReason> {
    if is_root {
        return Some(StackingContextReason::Root);
    }

    if is_positioned(style) && style.z_index.is_some() {
        return Some(StackingContextReason::PositionedWithZIndex);
    }

    if matches!(style.position, Position::Fixed) {
        return Some(StackingContextReason::FixedPositioning);
    }

    if matches!(style.position, Position::Sticky) {
        return Some(StackingContextReason::StickyPositioning);
    }

    if style.opacity < 1.0 {
        return Some(StackingContextReason::Opacity);
    }

    if !style.transform.is_empty() {
        return Some(StackingContextReason::Transform);
    }

    if !style.filter.is_empty() {
        return Some(StackingContextReason::Filter);
    }

    if !style.backdrop_filter.is_empty() {
        return Some(StackingContextReason::BackdropFilter);
    }

    if is_positioned(style)
        && (matches!(
            style.overflow_x,
            Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
        ) || matches!(
            style.overflow_y,
            Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
        ))
    {
        return Some(StackingContextReason::OverflowClip);
    }

    if let Some(parent) = parent_style {
        let parent_is_flex = matches!(parent.display, Display::Flex | Display::InlineFlex);
        let parent_is_grid = matches!(parent.display, Display::Grid | Display::InlineGrid);

        if parent_is_flex && style.z_index.is_some() {
            return Some(StackingContextReason::FlexItemWithZIndex);
        }
        if parent_is_grid && style.z_index.is_some() {
            return Some(StackingContextReason::GridItemWithZIndex);
        }
    }

    None
}

/// Checks if an element is positioned (not static)
fn is_positioned(style: &ComputedStyle) -> bool {
    !matches!(style.position, Position::Static)
}

/// Checks if an element is a float
///
/// Floats participate in layer 4 of the stacking order (between blocks and
/// inlines). Spec-wise floats are ignored for absolutely/fixed positioned
/// elements because their used value becomes `none`; we mirror that so
/// positioned elements stay in the positioned layer.
fn is_float(style: &ComputedStyle) -> bool {
    if matches!(style.position, Position::Absolute | Position::Fixed) {
        return false;
    }
    style.float.is_floating()
}

/// Checks if an element is inline-level
fn is_inline_level(style: &ComputedStyle, fragment: &FragmentNode) -> bool {
    // Check display property
    let is_inline_display = matches!(
        style.display,
        Display::Inline | Display::InlineBlock | Display::InlineFlex | Display::InlineGrid | Display::InlineTable
    );

    // Also check fragment content type
    let is_inline_content = matches!(
        fragment.content,
        FragmentContent::Inline { .. } | FragmentContent::Text { .. } | FragmentContent::Line { .. }
    );

    is_inline_display || is_inline_content
}

/// Builds a stacking context tree from a fragment tree
///
/// This function traverses the fragment tree and builds a corresponding
/// stacking context tree that can be used for correct paint ordering.
///
/// # Arguments
///
/// * `root` - The root fragment node
/// * `root_style` - Optional style for the root element
/// * `is_root_context` - Whether this is the document root
///
/// # Returns
///
/// A `StackingContext` representing the stacking context tree
///
/// # Example
///
/// ```ignore
/// use fastrender::paint::stacking::build_stacking_tree;
///
/// let root_fragment = /* ... */;
/// let stacking_tree = build_stacking_tree(&root_fragment, None, true);
/// ```
pub fn build_stacking_tree(
    root: &FragmentNode,
    root_style: Option<&ComputedStyle>,
    is_root_context: bool,
) -> StackingContext {
    let mut tree_order_counter = 0;
    let mut context = build_stacking_tree_internal(root, root_style, None, is_root_context, &mut tree_order_counter);

    // Sort all children by z-index
    context.sort_children();

    // Compute bounds
    context.compute_bounds();

    context
}

/// Internal recursive function to build stacking context tree
fn build_stacking_tree_internal(
    fragment: &FragmentNode,
    style: Option<&ComputedStyle>,
    parent_style: Option<&ComputedStyle>,
    is_root: bool,
    tree_order: &mut usize,
) -> StackingContext {
    let current_order = *tree_order;
    *tree_order += 1;

    // Check if this fragment creates a stacking context
    let creates_context = if let Some(s) = style {
        creates_stacking_context(s, parent_style, is_root)
    } else {
        is_root
    };

    if creates_context {
        // Create a new stacking context
        let z_index = style.and_then(|s| s.z_index).unwrap_or(0);
        let reason = style
            .and_then(|s| get_stacking_context_reason(s, parent_style, is_root))
            .unwrap_or(StackingContextReason::Root);

        let mut context = StackingContext::with_reason(z_index, reason, current_order);

        // Add the root fragment
        context.fragments.push(fragment.clone());

        // Process children
        for child in &fragment.children {
            let child_context = build_stacking_tree_internal(
                child, None, // We don't have style for children without external mapping
                style, false, tree_order,
            );

            let child_creates_context =
                child_context.reason != StackingContextReason::Root || child_context.z_index != 0;

            if child_creates_context {
                // Child has its own stacking context structure
                context.add_child(child_context);
            } else {
                // Propagate any nested child contexts upward
                if !child_context.children.is_empty() {
                    context.children.extend(child_context.children);
                }
                // Keep the direct child in the appropriate layer; children will be painted via recursion
                context.add_fragment_to_layer(child.clone(), None);
            }
        }

        context
    } else {
        // Don't create a new stacking context, but still process for layer classification
        let mut context = StackingContext::new(0);
        context.tree_order = current_order;

        // Classify this fragment into appropriate layer
        if let Some(s) = style {
            context.add_fragment_to_layer(fragment.clone(), Some(s));
        } else {
            // Classify based on fragment content
            match &fragment.content {
                FragmentContent::Text { .. } | FragmentContent::Inline { .. } | FragmentContent::Line { .. } => {
                    context.layer5_inlines.push(fragment.clone());
                }
                _ => {
                    context.layer3_blocks.push(fragment.clone());
                }
            }
        }

        // Process children
        for child in &fragment.children {
            let child_context = build_stacking_tree_internal(child, None, style, false, tree_order);

            let child_creates_context =
                child_context.reason != StackingContextReason::Root || child_context.z_index != 0;

            if child_creates_context {
                context.add_child(child_context);
            } else {
                if !child_context.children.is_empty() {
                    context.children.extend(child_context.children);
                }
                context.add_fragment_to_layer(child.clone(), None);
            }
        }

        context
    }
}

/// Builds a stacking context tree with style information from a styled tree
///
/// This version takes a style lookup function to get ComputedStyle for each fragment.
///
/// # Arguments
///
/// * `root` - The root fragment node
/// * `get_style` - Function to look up style for a fragment (by box_id or other means)
///
/// # Returns
///
/// A `StackingContext` representing the stacking context tree
pub fn build_stacking_tree_with_styles<F>(root: &FragmentNode, get_style: F) -> StackingContext
where
    F: Fn(&FragmentNode) -> Option<Arc<ComputedStyle>> + Clone,
{
    let root_style = get_style(root);
    let mut tree_order_counter = 0;

    let mut context = build_stacking_tree_with_styles_internal(
        root,
        root_style.as_ref().map(|s| s.as_ref()),
        None,
        true,
        &mut tree_order_counter,
        &get_style,
    );

    context.sort_children();
    context.compute_bounds();
    context
}

/// Builds a stacking context tree from a fragment tree using the fragment's embedded styles.
pub fn build_stacking_tree_from_fragment_tree(root: &FragmentNode) -> StackingContext {
    build_stacking_tree_with_styles(root, |fragment| fragment.style.clone())
}

/// Internal recursive function to build stacking context tree with styles
fn build_stacking_tree_with_styles_internal<F>(
    fragment: &FragmentNode,
    style: Option<&ComputedStyle>,
    parent_style: Option<&ComputedStyle>,
    is_root: bool,
    tree_order: &mut usize,
    get_style: &F,
) -> StackingContext
where
    F: Fn(&FragmentNode) -> Option<Arc<ComputedStyle>>,
{
    let current_order = *tree_order;
    *tree_order += 1;

    if let Some(style) = style {
        if !matches!(style.visibility, crate::style::computed::Visibility::Visible) {
            return StackingContext::new(0);
        }
    }

    let creates_context = if let Some(s) = style {
        creates_stacking_context(s, parent_style, is_root)
    } else {
        is_root
    };

    if creates_context {
        let z_index = style.and_then(|s| s.z_index).unwrap_or(0);
        let reason = style
            .and_then(|s| get_stacking_context_reason(s, parent_style, is_root))
            .unwrap_or(StackingContextReason::Root);

        let mut context = StackingContext::with_reason(z_index, reason, current_order);
        context.fragments.push(fragment.clone());

        for child in &fragment.children {
            let child_style = get_style(child);
            let child_context = build_stacking_tree_with_styles_internal(
                child,
                child_style.as_ref().map(|s| s.as_ref()),
                style,
                false,
                tree_order,
                get_style,
            );

            let child_creates_context =
                child_context.reason != StackingContextReason::Root || child_context.z_index != 0;

            if child_creates_context {
                context.add_child(child_context);
            } else {
                if !child_context.children.is_empty() {
                    context.children.extend(child_context.children);
                }
                context.add_fragment_to_layer(child.clone(), child_style.as_deref());
            }
        }

        context
    } else {
        let mut context = StackingContext::new(0);
        context.tree_order = current_order;

        if let Some(s) = style {
            context.add_fragment_to_layer(fragment.clone(), Some(s));
        } else {
            match &fragment.content {
                FragmentContent::Text { .. } | FragmentContent::Inline { .. } | FragmentContent::Line { .. } => {
                    context.layer5_inlines.push(fragment.clone());
                }
                _ => {
                    context.layer3_blocks.push(fragment.clone());
                }
            }
        }

        for child in &fragment.children {
            let child_style = get_style(child);
            let child_context = build_stacking_tree_with_styles_internal(
                child,
                child_style.as_ref().map(|s| s.as_ref()),
                style,
                false,
                tree_order,
                get_style,
            );

            let child_creates_context =
                child_context.reason != StackingContextReason::Root || child_context.z_index != 0;

            if child_creates_context {
                context.add_child(child_context);
            } else {
                if !child_context.children.is_empty() {
                    context.children.extend(child_context.children);
                }
                context.add_fragment_to_layer(child.clone(), child_style.as_deref());
            }
        }

        context
    }
}

/// Iterator for traversing stacking context in paint order
///
/// Yields fragments in the correct 7-layer paint order.
pub struct PaintOrderIterator<'a> {
    stack: Vec<PaintOrderItem<'a>>,
}

#[derive(Clone)]
enum PaintOrderItem<'a> {
    Context(&'a StackingContext),
    Layer3(&'a [FragmentNode], usize),
    Layer4(&'a [FragmentNode], usize),
    Layer5(&'a [FragmentNode], usize),
    Layer6(&'a [FragmentNode], usize),
    Fragments(&'a [FragmentNode], usize),
    NegativeChildren(Vec<&'a StackingContext>, usize),
    PositiveChildren(Vec<&'a StackingContext>, usize),
}

impl<'a> PaintOrderIterator<'a> {
    /// Creates a new paint order iterator for a stacking context
    pub fn new(context: &'a StackingContext) -> Self {
        Self {
            stack: vec![PaintOrderItem::Context(context)],
        }
    }
}

impl<'a> Iterator for PaintOrderIterator<'a> {
    type Item = &'a FragmentNode;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(item) = self.stack.pop() {
            match item {
                PaintOrderItem::Context(ctx) => {
                    // Push items in reverse order (last pushed = first processed)
                    // Layer 7: Positive z-index children
                    let positive = ctx.positive_z_children();
                    if !positive.is_empty() {
                        self.stack.push(PaintOrderItem::PositiveChildren(positive, 0));
                    }

                    // Layer 6: Positioned with z-index 0 or auto
                    if !ctx.layer6_positioned.is_empty() {
                        self.stack.push(PaintOrderItem::Layer6(&ctx.layer6_positioned, 0));
                    }

                    // Layer 5: Inline-level
                    if !ctx.layer5_inlines.is_empty() {
                        self.stack.push(PaintOrderItem::Layer5(&ctx.layer5_inlines, 0));
                    }

                    // Layer 4: Floats
                    if !ctx.layer4_floats.is_empty() {
                        self.stack.push(PaintOrderItem::Layer4(&ctx.layer4_floats, 0));
                    }

                    // Layer 3: Block-level
                    if !ctx.layer3_blocks.is_empty() {
                        self.stack.push(PaintOrderItem::Layer3(&ctx.layer3_blocks, 0));
                    }

                    // Layer 2: Negative z-index children
                    let negative = ctx.negative_z_children();
                    if !negative.is_empty() {
                        self.stack.push(PaintOrderItem::NegativeChildren(negative, 0));
                    }

                    // Layer 1: Background and borders (root fragments)
                    if !ctx.fragments.is_empty() {
                        self.stack.push(PaintOrderItem::Fragments(&ctx.fragments, 0));
                    }
                }
                PaintOrderItem::Fragments(fragments, idx) => {
                    if idx < fragments.len() {
                        self.stack.push(PaintOrderItem::Fragments(fragments, idx + 1));
                        return Some(&fragments[idx]);
                    }
                }
                PaintOrderItem::Layer3(fragments, idx) => {
                    if idx < fragments.len() {
                        self.stack.push(PaintOrderItem::Layer3(fragments, idx + 1));
                        return Some(&fragments[idx]);
                    }
                }
                PaintOrderItem::Layer4(fragments, idx) => {
                    if idx < fragments.len() {
                        self.stack.push(PaintOrderItem::Layer4(fragments, idx + 1));
                        return Some(&fragments[idx]);
                    }
                }
                PaintOrderItem::Layer5(fragments, idx) => {
                    if idx < fragments.len() {
                        self.stack.push(PaintOrderItem::Layer5(fragments, idx + 1));
                        return Some(&fragments[idx]);
                    }
                }
                PaintOrderItem::Layer6(fragments, idx) => {
                    if idx < fragments.len() {
                        self.stack.push(PaintOrderItem::Layer6(fragments, idx + 1));
                        return Some(&fragments[idx]);
                    }
                }
                PaintOrderItem::NegativeChildren(children, idx) => {
                    if idx < children.len() {
                        self.stack
                            .push(PaintOrderItem::NegativeChildren(children.clone(), idx + 1));
                        self.stack.push(PaintOrderItem::Context(children[idx]));
                    }
                }
                PaintOrderItem::PositiveChildren(children, idx) => {
                    if idx < children.len() {
                        self.stack
                            .push(PaintOrderItem::PositiveChildren(children.clone(), idx + 1));
                        self.stack.push(PaintOrderItem::Context(children[idx]));
                    }
                }
            }
        }
        None
    }
}

impl StackingContext {
    /// Returns an iterator that yields fragments in paint order
    pub fn iter_paint_order(&self) -> PaintOrderIterator<'_> {
        PaintOrderIterator::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Rect;

    // Helper function to create a simple fragment
    fn create_block_fragment(x: f32, y: f32, width: f32, height: f32) -> FragmentNode {
        FragmentNode::new_block(Rect::from_xywh(x, y, width, height), vec![])
    }

    fn create_text_fragment(x: f32, y: f32, width: f32, height: f32, text: &str) -> FragmentNode {
        FragmentNode::new_text(Rect::from_xywh(x, y, width, height), text.to_string(), 12.0)
    }

    // Stacking context creation tests

    #[test]
    fn test_creates_stacking_context_root() {
        let style = ComputedStyle::default();
        assert!(creates_stacking_context(&style, None, true));
    }

    #[test]
    fn test_creates_stacking_context_not_root() {
        let style = ComputedStyle::default();
        assert!(!creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_positioned_with_z_index() {
        let mut style = ComputedStyle::default();
        style.position = Position::Relative;
        style.z_index = Some(1);
        assert!(creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_positioned_with_zero_z_index() {
        let mut style = ComputedStyle::default();
        style.position = Position::Relative;
        style.z_index = Some(0); // explicit zero still creates stacking context
        assert!(creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_fixed() {
        let mut style = ComputedStyle::default();
        style.position = Position::Fixed;
        assert!(creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_sticky() {
        let mut style = ComputedStyle::default();
        style.position = Position::Sticky;
        assert!(creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_opacity() {
        let mut style = ComputedStyle::default();
        style.opacity = 0.5;
        assert!(creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_opacity_zero() {
        let mut style = ComputedStyle::default();
        style.opacity = 0.0;
        assert!(creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_opacity_one() {
        let mut style = ComputedStyle::default();
        style.opacity = 1.0;
        assert!(!creates_stacking_context(&style, None, false));
    }

    #[test]
    fn test_creates_stacking_context_flex_item_with_z_index() {
        let mut parent_style = ComputedStyle::default();
        parent_style.display = Display::Flex;

        let mut child_style = ComputedStyle::default();
        child_style.z_index = Some(1);

        assert!(creates_stacking_context(&child_style, Some(&parent_style), false));
    }

    #[test]
    fn test_creates_stacking_context_grid_item_with_z_index() {
        let mut parent_style = ComputedStyle::default();
        parent_style.display = Display::Grid;

        let mut child_style = ComputedStyle::default();
        child_style.z_index = Some(1);

        assert!(creates_stacking_context(&child_style, Some(&parent_style), false));
    }

    // StackingContext struct tests

    #[test]
    fn test_stacking_context_new() {
        let sc = StackingContext::new(5);
        assert_eq!(sc.z_index, 5);
        assert!(sc.children.is_empty());
        assert!(sc.fragments.is_empty());
    }

    #[test]
    fn test_stacking_context_root() {
        let sc = StackingContext::root();
        assert_eq!(sc.z_index, 0);
        assert_eq!(sc.reason, StackingContextReason::Root);
    }

    #[test]
    fn test_stacking_context_with_reason() {
        let sc = StackingContext::with_reason(10, StackingContextReason::Opacity, 5);
        assert_eq!(sc.z_index, 10);
        assert_eq!(sc.reason, StackingContextReason::Opacity);
        assert_eq!(sc.tree_order, 5);
    }

    #[test]
    fn test_stacking_context_add_child() {
        let mut parent = StackingContext::new(0);
        let child = StackingContext::new(1);
        parent.add_child(child);
        assert_eq!(parent.children.len(), 1);
        assert_eq!(parent.children[0].z_index, 1);
    }

    #[test]
    fn test_stacking_context_negative_z_children() {
        let mut parent = StackingContext::new(0);
        parent.add_child(StackingContext::with_reason(
            -5,
            StackingContextReason::PositionedWithZIndex,
            1,
        ));
        parent.add_child(StackingContext::with_reason(
            -1,
            StackingContextReason::PositionedWithZIndex,
            2,
        ));
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Root, 3));
        parent.add_child(StackingContext::with_reason(
            1,
            StackingContextReason::PositionedWithZIndex,
            4,
        ));

        let negative = parent.negative_z_children();
        assert_eq!(negative.len(), 2);
        assert_eq!(negative[0].z_index, -5); // Most negative first
        assert_eq!(negative[1].z_index, -1);
    }

    #[test]
    fn test_stacking_context_positive_z_children() {
        let mut parent = StackingContext::new(0);
        parent.add_child(StackingContext::with_reason(
            -1,
            StackingContextReason::PositionedWithZIndex,
            1,
        ));
        parent.add_child(StackingContext::with_reason(
            5,
            StackingContextReason::PositionedWithZIndex,
            2,
        ));
        parent.add_child(StackingContext::with_reason(
            1,
            StackingContextReason::PositionedWithZIndex,
            3,
        ));
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Root, 4));

        let positive = parent.positive_z_children();
        assert_eq!(positive.len(), 2);
        assert_eq!(positive[0].z_index, 1); // Least positive first
        assert_eq!(positive[1].z_index, 5);
    }

    #[test]
    fn test_stacking_context_zero_z_children() {
        let mut parent = StackingContext::new(0);
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Opacity, 1));
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Transform, 2));
        parent.add_child(StackingContext::with_reason(
            1,
            StackingContextReason::PositionedWithZIndex,
            3,
        ));

        let zero = parent.zero_z_children();
        assert_eq!(zero.len(), 2);
        // Should be in tree order
        assert_eq!(zero[0].tree_order, 1);
        assert_eq!(zero[1].tree_order, 2);
    }

    #[test]
    fn test_stacking_context_sort_children() {
        let mut parent = StackingContext::new(0);
        parent.add_child(StackingContext::with_reason(
            5,
            StackingContextReason::PositionedWithZIndex,
            1,
        ));
        parent.add_child(StackingContext::with_reason(
            -2,
            StackingContextReason::PositionedWithZIndex,
            2,
        ));
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Opacity, 3));
        parent.add_child(StackingContext::with_reason(
            1,
            StackingContextReason::PositionedWithZIndex,
            4,
        ));

        parent.sort_children();

        assert_eq!(parent.children[0].z_index, -2);
        assert_eq!(parent.children[1].z_index, 0);
        assert_eq!(parent.children[2].z_index, 1);
        assert_eq!(parent.children[3].z_index, 5);
    }

    #[test]
    fn test_stacking_context_sort_children_equal_z_index_uses_tree_order() {
        let mut parent = StackingContext::new(0);
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Opacity, 3));
        parent.add_child(StackingContext::with_reason(0, StackingContextReason::Transform, 1));
        parent.add_child(StackingContext::with_reason(
            0,
            StackingContextReason::FixedPositioning,
            2,
        ));

        parent.sort_children();

        // Should be sorted by tree order for equal z-index
        assert_eq!(parent.children[0].tree_order, 1);
        assert_eq!(parent.children[1].tree_order, 2);
        assert_eq!(parent.children[2].tree_order, 3);
    }

    // Build stacking tree tests

    #[test]
    fn test_build_stacking_tree_single_fragment() {
        let fragment = create_block_fragment(0.0, 0.0, 100.0, 100.0);
        let tree = build_stacking_tree(&fragment, None, true);

        assert_eq!(tree.z_index, 0);
        assert_eq!(tree.reason, StackingContextReason::Root);
        assert!(!tree.fragments.is_empty());
    }

    #[test]
    fn test_build_stacking_tree_with_children() {
        let child1 = create_block_fragment(0.0, 0.0, 50.0, 50.0);
        let child2 = create_block_fragment(50.0, 0.0, 50.0, 50.0);
        let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![child1, child2]);

        let tree = build_stacking_tree(&root, None, true);

        assert_eq!(tree.reason, StackingContextReason::Root);
        // Root fragment + children classified
        assert!(tree.total_fragment_count() >= 3);
    }

    #[test]
    fn test_build_stacking_tree_with_style() {
        let fragment = create_block_fragment(0.0, 0.0, 100.0, 100.0);

        let mut style = ComputedStyle::default();
        style.opacity = 0.5;

        let tree = build_stacking_tree(&fragment, Some(&style), false);
        assert_eq!(tree.reason, StackingContextReason::Opacity);
    }

    // Fragment count tests

    #[test]
    fn test_fragment_count() {
        let mut sc = StackingContext::new(0);
        sc.fragments.push(create_block_fragment(0.0, 0.0, 10.0, 10.0));
        sc.layer3_blocks.push(create_block_fragment(0.0, 0.0, 10.0, 10.0));
        sc.layer5_inlines
            .push(create_text_fragment(0.0, 0.0, 10.0, 10.0, "test"));

        assert_eq!(sc.fragment_count(), 3);
    }

    #[test]
    fn test_total_fragment_count() {
        let mut parent = StackingContext::new(0);
        parent.fragments.push(create_block_fragment(0.0, 0.0, 10.0, 10.0));

        let mut child = StackingContext::new(1);
        child.fragments.push(create_block_fragment(0.0, 0.0, 10.0, 10.0));
        child.layer3_blocks.push(create_block_fragment(0.0, 0.0, 10.0, 10.0));

        parent.add_child(child);

        assert_eq!(parent.total_fragment_count(), 3);
    }

    // Paint order iterator tests

    #[test]
    fn test_paint_order_iterator_empty() {
        let sc = StackingContext::new(0);
        let count = sc.iter_paint_order().count();
        assert_eq!(count, 0);
    }

    #[test]
    fn test_paint_order_iterator_single_fragment() {
        let mut sc = StackingContext::new(0);
        sc.fragments.push(create_block_fragment(0.0, 0.0, 100.0, 100.0));

        let count = sc.iter_paint_order().count();
        assert_eq!(count, 1);
    }

    #[test]
    fn test_paint_order_iterator_multiple_layers() {
        let mut sc = StackingContext::new(0);
        sc.fragments.push(create_block_fragment(0.0, 0.0, 10.0, 10.0)); // Layer 1
        sc.layer3_blocks.push(create_block_fragment(10.0, 0.0, 10.0, 10.0)); // Layer 3
        sc.layer5_inlines
            .push(create_text_fragment(20.0, 0.0, 10.0, 10.0, "test")); // Layer 5

        let fragments: Vec<_> = sc.iter_paint_order().collect();
        assert_eq!(fragments.len(), 3);

        // Verify order: Layer 1 first, then Layer 3, then Layer 5
        assert_eq!(fragments[0].bounds.x(), 0.0); // Layer 1
        assert_eq!(fragments[1].bounds.x(), 10.0); // Layer 3
        assert_eq!(fragments[2].bounds.x(), 20.0); // Layer 5
    }

    #[test]
    fn test_paint_order_with_z_index_children() {
        let mut root = StackingContext::new(0);
        root.fragments.push(create_block_fragment(0.0, 0.0, 100.0, 100.0));

        // Negative z-index child
        let mut neg_child = StackingContext::with_reason(-1, StackingContextReason::PositionedWithZIndex, 1);
        neg_child.fragments.push(create_block_fragment(10.0, 10.0, 20.0, 20.0));
        root.add_child(neg_child);

        // Positive z-index child
        let mut pos_child = StackingContext::with_reason(1, StackingContextReason::PositionedWithZIndex, 2);
        pos_child.fragments.push(create_block_fragment(30.0, 30.0, 20.0, 20.0));
        root.add_child(pos_child);

        let fragments: Vec<_> = root.iter_paint_order().collect();
        assert_eq!(fragments.len(), 3);

        // Order should be: root background, negative child, positive child
        assert_eq!(fragments[0].bounds.x(), 0.0); // Root (Layer 1)
        assert_eq!(fragments[1].bounds.x(), 10.0); // Negative child (Layer 2)
        assert_eq!(fragments[2].bounds.x(), 30.0); // Positive child (Layer 7)
    }

    // Reason tests

    #[test]
    fn test_get_stacking_context_reason_root() {
        let style = ComputedStyle::default();
        let reason = get_stacking_context_reason(&style, None, true);
        assert_eq!(reason, Some(StackingContextReason::Root));
    }

    #[test]
    fn test_get_stacking_context_reason_opacity() {
        let mut style = ComputedStyle::default();
        style.opacity = 0.5;
        let reason = get_stacking_context_reason(&style, None, false);
        assert_eq!(reason, Some(StackingContextReason::Opacity));
    }

    #[test]
    fn test_get_stacking_context_reason_positioned_with_z_index() {
        let mut style = ComputedStyle::default();
        style.position = Position::Relative;
        style.z_index = Some(5);
        let reason = get_stacking_context_reason(&style, None, false);
        assert_eq!(reason, Some(StackingContextReason::PositionedWithZIndex));
    }

    #[test]
    fn test_get_stacking_context_reason_none() {
        let style = ComputedStyle::default();
        let reason = get_stacking_context_reason(&style, None, false);
        assert_eq!(reason, None);
    }

    // Bounds computation tests

    #[test]
    fn test_compute_bounds() {
        let mut sc = StackingContext::new(0);
        sc.fragments.push(create_block_fragment(0.0, 0.0, 50.0, 50.0));
        sc.layer3_blocks.push(create_block_fragment(40.0, 40.0, 60.0, 60.0));

        sc.compute_bounds();

        // Should encompass both fragments: (0,0) to (100, 100)
        assert_eq!(sc.bounds.min_x(), 0.0);
        assert_eq!(sc.bounds.min_y(), 0.0);
        assert_eq!(sc.bounds.max_x(), 100.0);
        assert_eq!(sc.bounds.max_y(), 100.0);
    }

    // Layer classification tests

    #[test]
    fn test_add_fragment_to_layer_block() {
        let mut sc = StackingContext::new(0);
        let fragment = create_block_fragment(0.0, 0.0, 100.0, 100.0);
        let mut style = ComputedStyle::default();
        style.display = Display::Block; // Default is Inline, need Block for block layer

        sc.add_fragment_to_layer(fragment, Some(&style));

        assert_eq!(sc.layer3_blocks.len(), 1);
    }

    #[test]
    fn test_add_fragment_to_layer_inline() {
        let mut sc = StackingContext::new(0);
        let fragment = create_text_fragment(0.0, 0.0, 50.0, 20.0, "test");
        let mut style = ComputedStyle::default();
        style.display = Display::Inline;

        sc.add_fragment_to_layer(fragment, Some(&style));

        assert_eq!(sc.layer5_inlines.len(), 1);
    }

    #[test]
    fn test_add_fragment_to_layer_positioned() {
        let mut sc = StackingContext::new(0);
        let fragment = create_block_fragment(0.0, 0.0, 100.0, 100.0);
        let mut style = ComputedStyle::default();
        style.position = Position::Relative;
        // z_index = 0 (default), so goes to layer 6

        sc.add_fragment_to_layer(fragment, Some(&style));

        assert_eq!(sc.layer6_positioned.len(), 1);
    }
}
