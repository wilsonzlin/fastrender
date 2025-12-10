//! Flexbox Formatting Context (via Taffy)
//!
//! This module implements the Flexbox layout algorithm by delegating to the Taffy library.
//! Taffy is a battle-tested layout library that implements the CSS Flexbox specification.
//!
//! # Design
//!
//! The FlexFormattingContext acts as a thin wrapper around Taffy's flexbox implementation:
//! 1. Convert BoxNode tree to Taffy tree (with Taffy styles)
//! 2. Run Taffy's `compute_layout()` algorithm
//! 3. Convert Taffy's layout results back to FragmentNode tree
//!
//! # Why Taffy?
//!
//! - Complete CSS Flexbox spec compliance
//! - Well-tested against Web Platform Tests
//! - Active maintenance by Dioxus team
//! - Saves months of implementation time
//!
//! # References
//!
//! - CSS Flexible Box Layout Module Level 1: <https://www.w3.org/TR/css-flexbox-1/>
//! - Taffy documentation: <https://docs.rs/taffy/>

use std::collections::HashMap;

use crate::geometry::{Point, Rect, Size};
use crate::layout::constraints::{AvailableSpace as CrateAvailableSpace, LayoutConstraints};
use crate::layout::formatting_context::{FormattingContext, IntrinsicSizingMode, LayoutError};
use crate::style::display::Display;
use crate::style::types::{
    AlignContent, AlignItems, AspectRatio, BoxSizing, FlexBasis, FlexDirection, FlexWrap, JustifyContent,
};
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;
use crate::tree::box_tree::BoxNode;
use crate::tree::fragment_tree::FragmentNode;

use taffy::prelude::*;
use taffy::TaffyTree;

#[derive(Clone, Copy)]
enum Axis {
    Horizontal,
    Vertical,
}

/// Flexbox Formatting Context
///
/// Delegates layout to Taffy's flexbox algorithm. This is a stateless struct
/// that creates a fresh Taffy tree for each layout operation to avoid state issues.
///
/// # Thread Safety
///
/// This struct is `Send + Sync` as required by the `FormattingContext` trait.
/// Each layout operation creates its own TaffyTree instance, ensuring thread safety.
///
/// # Example
///
/// ```ignore
/// use fastrender::layout::contexts::FlexFormattingContext;
/// use fastrender::LayoutConstraints;
/// use fastrender::tree::BoxNode;
///
/// let fc = FlexFormattingContext::new();
/// let constraints = LayoutConstraints::definite(800.0, 600.0);
/// let fragment = fc.layout(&box_node, &constraints)?;
/// ```
#[derive(Debug, Clone)]
pub struct FlexFormattingContext {
    /// Viewport size used for resolving viewport-relative units inside Taffy conversion.
    viewport_size: Size,
}

impl FlexFormattingContext {
    /// Creates a new FlexFormattingContext
    pub fn new() -> Self {
        Self {
            viewport_size: Size::new(800.0, 600.0),
        }
    }

    pub fn with_viewport(viewport_size: Size) -> Self {
        Self { viewport_size }
    }
}

impl Default for FlexFormattingContext {
    fn default() -> Self {
        Self::new()
    }
}

impl FormattingContext for FlexFormattingContext {
    /// Lays out a flex container and its children using Taffy
    ///
    /// # Process
    ///
    /// 1. Build a Taffy tree from the BoxNode tree
    /// 2. Set available space constraints
    /// 3. Run Taffy's compute_layout()
    /// 4. Convert Taffy layout results back to FragmentNode tree
    fn layout(&self, box_node: &BoxNode, constraints: &LayoutConstraints) -> Result<FragmentNode, LayoutError> {
        // Create a fresh Taffy tree for this layout
        let mut taffy_tree: TaffyTree<()> = TaffyTree::new();

        // Phase 1: Build Taffy tree from BoxNode tree
        let mut node_map: HashMap<*const BoxNode, NodeId> = HashMap::new();
        let root_node = self.build_taffy_tree(&mut taffy_tree, box_node, &mut node_map)?;

        // Phase 2: Compute layout using Taffy
        let available_space = self.constraints_to_available_space(constraints);
        taffy_tree
            .compute_layout(root_node, available_space)
            .map_err(|e| LayoutError::MissingContext(format!("Taffy layout failed: {:?}", e)))?;

        // Phase 3: Convert Taffy layout back to FragmentNode
        let fragment = self.taffy_to_fragment(&taffy_tree, root_node, box_node, &node_map)?;

        Ok(fragment)
    }

    /// Computes intrinsic size by running Taffy with appropriate constraints
    fn compute_intrinsic_inline_size(&self, box_node: &BoxNode, mode: IntrinsicSizingMode) -> Result<f32, LayoutError> {
        // Create a fresh Taffy tree
        let mut taffy_tree: TaffyTree<()> = TaffyTree::new();

        // Build Taffy tree
        let mut node_map: HashMap<*const BoxNode, NodeId> = HashMap::new();
        let root_node = self.build_taffy_tree(&mut taffy_tree, box_node, &mut node_map)?;

        // Compute layout with appropriate constraints
        let available_space = match mode {
            IntrinsicSizingMode::MinContent => taffy::geometry::Size {
                width: AvailableSpace::MinContent,
                height: AvailableSpace::MaxContent,
            },
            IntrinsicSizingMode::MaxContent => taffy::geometry::Size {
                width: AvailableSpace::MaxContent,
                height: AvailableSpace::MaxContent,
            },
        };

        taffy_tree
            .compute_layout(root_node, available_space)
            .map_err(|e| LayoutError::MissingContext(format!("Taffy intrinsic size failed: {:?}", e)))?;

        // Get the computed width
        let layout = taffy_tree
            .layout(root_node)
            .map_err(|e| LayoutError::MissingContext(format!("Failed to get layout: {:?}", e)))?;

        Ok(layout.size.width)
    }
}

impl FlexFormattingContext {
    /// Builds a Taffy tree from a BoxNode tree
    ///
    /// Returns the root NodeId and populates the node_map for later lookups.
    fn build_taffy_tree(
        &self,
        taffy_tree: &mut TaffyTree<()>,
        box_node: &BoxNode,
        node_map: &mut HashMap<*const BoxNode, NodeId>,
    ) -> Result<NodeId, LayoutError> {
        self.build_taffy_tree_inner(taffy_tree, box_node, node_map, true)
    }

    /// Internal tree builder that tracks whether we're at the root
    fn build_taffy_tree_inner(
        &self,
        taffy_tree: &mut TaffyTree<()>,
        box_node: &BoxNode,
        node_map: &mut HashMap<*const BoxNode, NodeId>,
        is_root: bool,
    ) -> Result<NodeId, LayoutError> {
        // Convert style to Taffy style
        let taffy_style = self.computed_style_to_taffy(&box_node.style, is_root);

        // Create Taffy node
        let taffy_node = if box_node.children.is_empty() {
            // Leaf node
            taffy_tree
                .new_leaf(taffy_style)
                .map_err(|e| LayoutError::MissingContext(format!("Failed to create Taffy leaf: {:?}", e)))?
        } else {
            // Create children first (not root)
            let mut taffy_children = Vec::with_capacity(box_node.children.len());
            for child in &box_node.children {
                let child_node = self.build_taffy_tree_inner(taffy_tree, child, node_map, false)?;
                taffy_children.push(child_node);
            }

            // Create parent with children
            taffy_tree
                .new_with_children(taffy_style, &taffy_children)
                .map_err(|e| LayoutError::MissingContext(format!("Failed to create Taffy node: {:?}", e)))?
        };

        // Record mapping for later fragment conversion
        node_map.insert(box_node as *const BoxNode, taffy_node);

        Ok(taffy_node)
    }

    /// Converts our ComputedStyle to Taffy's Style
    ///
    /// The `is_root` flag indicates if this is the root flex container.
    /// For the root, we use Flex display; for children, we use Block.
    fn computed_style_to_taffy(&self, style: &ComputedStyle, is_root: bool) -> taffy::style::Style {
        taffy::style::Style {
            // Display mode - only root is Flex, children are Block (flex items)
            display: self.display_to_taffy(style, is_root),

            // Flex container properties
            flex_direction: self.flex_direction_to_taffy(style.flex_direction),
            flex_wrap: self.flex_wrap_to_taffy(style.flex_wrap),
            justify_content: self.justify_content_to_taffy(style.justify_content),
            align_items: self.align_items_to_taffy(style.align_items),
            align_content: self.align_content_to_taffy(style.align_content),
            align_self: self.align_self_to_taffy(style.align_self),
            justify_self: self.align_self_to_taffy(style.justify_self),
            justify_items: self.align_items_to_taffy(style.justify_items),

            // Gap
            gap: taffy::geometry::Size {
                width: self.length_to_taffy_lp(&style.grid_column_gap, style),
                height: self.length_to_taffy_lp(&style.grid_row_gap, style),
            },

            // Flex item properties
            flex_grow: style.flex_grow,
            flex_shrink: style.flex_shrink,
            flex_basis: self.flex_basis_to_taffy(&style.flex_basis, style),

            // Sizing - for root flex container without explicit size, use 100%
            // to fill the available space (block-level behavior)
            size: self.compute_size(style, is_root),
            min_size: taffy::geometry::Size {
                width: self.length_option_to_dimension_box_sizing(style.min_width.as_ref(), style, Axis::Horizontal),
                height: self.length_option_to_dimension_box_sizing(style.min_height.as_ref(), style, Axis::Vertical),
            },
            max_size: taffy::geometry::Size {
                width: self.length_option_to_dimension_box_sizing(style.max_width.as_ref(), style, Axis::Horizontal),
                height: self.length_option_to_dimension_box_sizing(style.max_height.as_ref(), style, Axis::Vertical),
            },

            // Spacing
            padding: taffy::geometry::Rect {
                left: self.length_to_taffy_lp(&style.padding_left, style),
                right: self.length_to_taffy_lp(&style.padding_right, style),
                top: self.length_to_taffy_lp(&style.padding_top, style),
                bottom: self.length_to_taffy_lp(&style.padding_bottom, style),
            },
            margin: taffy::geometry::Rect {
                left: self.length_option_to_lpa(style.margin_left.as_ref(), style),
                right: self.length_option_to_lpa(style.margin_right.as_ref(), style),
                top: self.length_option_to_lpa(style.margin_top.as_ref(), style),
                bottom: self.length_option_to_lpa(style.margin_bottom.as_ref(), style),
            },
            border: taffy::geometry::Rect {
                left: self.length_to_taffy_lp(&style.border_left_width, style),
                right: self.length_to_taffy_lp(&style.border_right_width, style),
                top: self.length_to_taffy_lp(&style.border_top_width, style),
                bottom: self.length_to_taffy_lp(&style.border_bottom_width, style),
            },
            aspect_ratio: self.aspect_ratio_to_taffy(style.aspect_ratio),

            ..Default::default()
        }
    }

    /// Computes the size for a node
    ///
    /// For the root flex container without explicit size, we use 100% to fill
    /// available space (simulating block-level behavior).
    fn compute_size(&self, style: &ComputedStyle, is_root: bool) -> taffy::geometry::Size<Dimension> {
        let width = match style.width.as_ref() {
            Some(len) => self.dimension_for_box_sizing(len, style, Axis::Horizontal),
            None if is_root => {
                // Root flex container without explicit width: expand to fill
                // available space (100% of containing block)
                Dimension::percent(1.0)
            }
            None => Dimension::auto(),
        };

        let height = match style.height.as_ref() {
            Some(len) => self.dimension_for_box_sizing(len, style, Axis::Vertical),
            None => Dimension::auto(), // Height always auto unless specified
        };

        taffy::geometry::Size { width, height }
    }

    /// Converts Taffy layout back to FragmentNode tree
    #[allow(clippy::only_used_in_recursion)]
    fn taffy_to_fragment(
        &self,
        taffy_tree: &TaffyTree<()>,
        taffy_node: NodeId,
        box_node: &BoxNode,
        node_map: &HashMap<*const BoxNode, NodeId>,
    ) -> Result<FragmentNode, LayoutError> {
        // Get layout from Taffy
        let layout = taffy_tree
            .layout(taffy_node)
            .map_err(|e| LayoutError::MissingContext(format!("Failed to get Taffy layout: {:?}", e)))?;

        // Create fragment rect (Taffy uses relative coordinates)
        let rect = Rect::new(
            Point::new(layout.location.x, layout.location.y),
            Size::new(layout.size.width, layout.size.height),
        );

        // Convert children
        let mut children = Vec::with_capacity(box_node.children.len());
        for child_box in &box_node.children {
            if let Some(&child_taffy) = node_map.get(&(child_box as *const BoxNode)) {
                let child_fragment = self.taffy_to_fragment(taffy_tree, child_taffy, child_box, node_map)?;
                children.push(child_fragment);
            }
        }

        Ok(FragmentNode::new_block_styled(rect, children, box_node.style.clone()))
    }

    /// Converts layout constraints to Taffy available space
    fn constraints_to_available_space(&self, constraints: &LayoutConstraints) -> taffy::geometry::Size<AvailableSpace> {
        taffy::geometry::Size {
            width: match constraints.available_width {
                CrateAvailableSpace::Definite(w) => AvailableSpace::Definite(w),
                CrateAvailableSpace::MinContent => AvailableSpace::MinContent,
                CrateAvailableSpace::MaxContent => AvailableSpace::MaxContent,
                CrateAvailableSpace::Indefinite => AvailableSpace::MaxContent,
            },
            height: match constraints.available_height {
                CrateAvailableSpace::Definite(h) => AvailableSpace::Definite(h),
                CrateAvailableSpace::MinContent => AvailableSpace::MinContent,
                CrateAvailableSpace::MaxContent => AvailableSpace::MaxContent,
                CrateAvailableSpace::Indefinite => AvailableSpace::MaxContent,
            },
        }
    }

    // ==========================================================================
    // Type conversion helpers
    // ==========================================================================

    fn display_to_taffy(&self, style: &ComputedStyle, is_root: bool) -> taffy::style::Display {
        // Root container is always Flex (that's why we're using FlexFormattingContext)
        // Children use their actual display mode, defaulting to Block for flex items
        if is_root {
            taffy::style::Display::Flex
        } else {
            // For children within a flex container, check if they're nested flex/grid
            match style.display {
                Display::Flex | Display::InlineFlex => taffy::style::Display::Flex,
                Display::Grid | Display::InlineGrid => taffy::style::Display::Grid,
                Display::None => taffy::style::Display::None,
                // Regular items become flex items with block-level sizing
                _ => taffy::style::Display::Block,
            }
        }
    }

    fn flex_direction_to_taffy(&self, dir: FlexDirection) -> taffy::style::FlexDirection {
        match dir {
            FlexDirection::Row => taffy::style::FlexDirection::Row,
            FlexDirection::RowReverse => taffy::style::FlexDirection::RowReverse,
            FlexDirection::Column => taffy::style::FlexDirection::Column,
            FlexDirection::ColumnReverse => taffy::style::FlexDirection::ColumnReverse,
        }
    }

    fn flex_wrap_to_taffy(&self, wrap: FlexWrap) -> taffy::style::FlexWrap {
        match wrap {
            FlexWrap::NoWrap => taffy::style::FlexWrap::NoWrap,
            FlexWrap::Wrap => taffy::style::FlexWrap::Wrap,
            FlexWrap::WrapReverse => taffy::style::FlexWrap::WrapReverse,
        }
    }

    fn justify_content_to_taffy(&self, justify: JustifyContent) -> Option<taffy::style::JustifyContent> {
        Some(match justify {
            JustifyContent::FlexStart => taffy::style::JustifyContent::FlexStart,
            JustifyContent::FlexEnd => taffy::style::JustifyContent::FlexEnd,
            JustifyContent::Center => taffy::style::JustifyContent::Center,
            JustifyContent::SpaceBetween => taffy::style::JustifyContent::SpaceBetween,
            JustifyContent::SpaceAround => taffy::style::JustifyContent::SpaceAround,
            JustifyContent::SpaceEvenly => taffy::style::JustifyContent::SpaceEvenly,
        })
    }

    fn align_items_to_taffy(&self, align: AlignItems) -> Option<taffy::style::AlignItems> {
        Some(match align {
            AlignItems::FlexStart => taffy::style::AlignItems::FlexStart,
            AlignItems::FlexEnd => taffy::style::AlignItems::FlexEnd,
            AlignItems::Center => taffy::style::AlignItems::Center,
            AlignItems::Baseline => taffy::style::AlignItems::Baseline,
            AlignItems::Stretch => taffy::style::AlignItems::Stretch,
        })
    }

    fn align_self_to_taffy(&self, align: Option<AlignItems>) -> Option<taffy::style::AlignItems> {
        align.and_then(|a| self.align_items_to_taffy(a))
    }

    fn align_content_to_taffy(&self, align: AlignContent) -> Option<taffy::style::AlignContent> {
        Some(match align {
            AlignContent::FlexStart => taffy::style::AlignContent::FlexStart,
            AlignContent::FlexEnd => taffy::style::AlignContent::FlexEnd,
            AlignContent::Center => taffy::style::AlignContent::Center,
            AlignContent::SpaceBetween => taffy::style::AlignContent::SpaceBetween,
            AlignContent::SpaceAround => taffy::style::AlignContent::SpaceAround,
            AlignContent::Stretch => taffy::style::AlignContent::Stretch,
        })
    }

    fn flex_basis_to_taffy(&self, basis: &FlexBasis, style: &ComputedStyle) -> Dimension {
        match basis {
            FlexBasis::Auto => Dimension::auto(),
            FlexBasis::Length(len) => self.length_to_dimension(len, style),
        }
    }

    fn horizontal_edges_px(&self, style: &ComputedStyle) -> Option<f32> {
        let left = self.resolve_length_px(&style.padding_left, style)?;
        let right = self.resolve_length_px(&style.padding_right, style)?;
        let bl = self.resolve_length_px(&style.border_left_width, style)?;
        let br = self.resolve_length_px(&style.border_right_width, style)?;
        Some(left + right + bl + br)
    }

    fn vertical_edges_px(&self, style: &ComputedStyle) -> Option<f32> {
        let top = self.resolve_length_px(&style.padding_top, style)?;
        let bottom = self.resolve_length_px(&style.padding_bottom, style)?;
        let bt = self.resolve_length_px(&style.border_top_width, style)?;
        let bb = self.resolve_length_px(&style.border_bottom_width, style)?;
        Some(top + bottom + bt + bb)
    }

    fn resolve_length_px(&self, len: &Length, style: &ComputedStyle) -> Option<f32> {
        match len.unit {
            LengthUnit::Percent => None,
            _ if len.unit.is_absolute() => Some(len.to_px()),
            u if u.is_viewport_relative() => {
                Some(len.resolve_with_viewport(self.viewport_size.width, self.viewport_size.height))
            }
            LengthUnit::Rem => Some(len.value * style.root_font_size),
            LengthUnit::Em => Some(len.value * style.font_size),
            LengthUnit::Ex => Some(len.value * style.font_size * 0.5),
            LengthUnit::Ch => Some(len.value * style.font_size * 0.5),
            _ => None,
        }
    }

    fn dimension_for_box_sizing(&self, len: &Length, style: &ComputedStyle, axis: Axis) -> Dimension {
        if style.box_sizing == BoxSizing::ContentBox {
            if let Some(edges) = match axis {
                Axis::Horizontal => self.horizontal_edges_px(style),
                Axis::Vertical => self.vertical_edges_px(style),
            } {
                if let Some(px) = self.resolve_length_px(len, style) {
                    return Dimension::length((px + edges).max(0.0));
                }
            }
        }
        self.length_to_dimension(len, style)
    }

    fn length_to_dimension(&self, len: &Length, style: &ComputedStyle) -> Dimension {
        match len.unit {
            LengthUnit::Px => Dimension::length(len.to_px()),
            LengthUnit::Percent => Dimension::percent(len.value / 100.0),
            _ => {
                if let Some(px) = self.resolve_length_px(len, style) {
                    Dimension::length(px)
                } else {
                    Dimension::length(len.to_px())
                }
            }
        }
    }

    fn length_option_to_dimension_box_sizing(
        &self,
        len: Option<&Length>,
        style: &ComputedStyle,
        axis: Axis,
    ) -> Dimension {
        match len {
            Some(l) => self.dimension_for_box_sizing(l, style, axis),
            None => Dimension::auto(),
        }
    }

    #[allow(dead_code)]
    fn length_option_to_dimension(&self, len: Option<&Length>, style: &ComputedStyle) -> Dimension {
        match len {
            Some(l) => self.length_to_dimension(l, style),
            None => Dimension::auto(),
        }
    }

    fn length_to_taffy_lp(&self, len: &Length, style: &ComputedStyle) -> LengthPercentage {
        match len.unit {
            LengthUnit::Percent => LengthPercentage::percent(len.value / 100.0),
            _ => {
                if let Some(px) = self.resolve_length_px(len, style) {
                    LengthPercentage::length(px)
                } else {
                    LengthPercentage::length(len.to_px())
                }
            }
        }
    }

    fn length_option_to_lpa(&self, len: Option<&Length>, style: &ComputedStyle) -> LengthPercentageAuto {
        match len {
            Some(l) => match l.unit {
                LengthUnit::Percent => LengthPercentageAuto::percent(l.value / 100.0),
                _ => {
                    if let Some(px) = self.resolve_length_px(l, style) {
                        LengthPercentageAuto::length(px)
                    } else {
                        LengthPercentageAuto::length(l.to_px())
                    }
                }
            },
            None => LengthPercentageAuto::auto(),
        }
    }

    fn aspect_ratio_to_taffy(&self, aspect_ratio: AspectRatio) -> Option<f32> {
        match aspect_ratio {
            AspectRatio::Auto => None,
            AspectRatio::Ratio(ratio) => Some(ratio),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::display::Display;
    use crate::style::display::FormattingContextType;
    use crate::style::types::{AlignItems, AspectRatio};
    use std::sync::Arc;

    fn create_flex_style() -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.display = Display::Flex;
        style.flex_direction = FlexDirection::Row;
        Arc::new(style)
    }

    fn create_item_style(width: f32, height: f32) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.width = Some(Length::px(width));
        style.height = Some(Length::px(height));
        Arc::new(style)
    }

    fn create_item_style_with_grow(width: f32, height: f32, grow: f32) -> Arc<ComputedStyle> {
        let mut style = ComputedStyle::default();
        style.width = Some(Length::px(width));
        style.height = Some(Length::px(height));
        style.flex_grow = grow;
        Arc::new(style)
    }

    #[test]
    fn test_flex_context_creation() {
        let _fc = FlexFormattingContext::new();
        let _fc_default = FlexFormattingContext::default();
        // Both methods should create valid contexts
        // (PhantomData<()> is zero-sized, so we just verify creation works)
    }

    #[test]
    fn test_basic_flex_row_layout() {
        let fc = FlexFormattingContext::new();

        // Create flex container with 3 children
        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            create_flex_style(),
            FormattingContextType::Flex,
            vec![item1, item2, item3],
        );

        let constraints = LayoutConstraints::definite(400.0, 600.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // Check that children are laid out horizontally
        assert_eq!(fragment.children.len(), 3);

        // Items should be positioned at x=0, 100, 200
        assert_eq!(fragment.children[0].bounds.x(), 0.0);
        assert_eq!(fragment.children[1].bounds.x(), 100.0);
        assert_eq!(fragment.children[2].bounds.x(), 200.0);

        // All items should have same y position
        assert_eq!(fragment.children[0].bounds.y(), 0.0);
        assert_eq!(fragment.children[1].bounds.y(), 0.0);
        assert_eq!(fragment.children[2].bounds.y(), 0.0);
    }

    #[test]
    fn test_flex_column_layout() {
        let fc = FlexFormattingContext::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Flex;
        container_style.flex_direction = FlexDirection::Column;

        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 75.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 25.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            Arc::new(container_style),
            FormattingContextType::Flex,
            vec![item1, item2, item3],
        );

        let constraints = LayoutConstraints::definite(400.0, 600.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // Check that children are laid out vertically
        assert_eq!(fragment.children.len(), 3);

        // Items should be positioned at y=0, 50, 125
        assert_eq!(fragment.children[0].bounds.y(), 0.0);
        assert_eq!(fragment.children[1].bounds.y(), 50.0);
        assert_eq!(fragment.children[2].bounds.y(), 125.0);

        // All items should have same x position
        assert_eq!(fragment.children[0].bounds.x(), 0.0);
        assert_eq!(fragment.children[1].bounds.x(), 0.0);
        assert_eq!(fragment.children[2].bounds.x(), 0.0);
    }

    #[test]
    fn test_flex_grow() {
        let fc = FlexFormattingContext::new();

        // Two items: one with flex-grow: 1, one without
        let item1 = BoxNode::new_block(
            create_item_style_with_grow(100.0, 50.0, 1.0),
            FormattingContextType::Block,
            vec![],
        );
        let item2 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![item1, item2]);

        let constraints = LayoutConstraints::definite(400.0, 600.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // First item should grow to fill available space
        // Container width: 400
        // Item2 base width: 100
        // Item1 should get: 400 - 100 = 300
        assert_eq!(fragment.children[0].bounds.width(), 300.0);
        assert_eq!(fragment.children[1].bounds.width(), 100.0);
    }

    #[test]
    fn test_flex_shrink() {
        let fc = FlexFormattingContext::new();

        // Two items, total wider than container
        let item1 = BoxNode::new_block(create_item_style(250.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(250.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![item1, item2]);

        // Container only 400px wide, but items total 500px
        let constraints = LayoutConstraints::definite(400.0, 600.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // Items should shrink equally (default flex-shrink: 1)
        // Deficit: 500 - 400 = 100
        // Each shrinks by 50
        assert_eq!(fragment.children[0].bounds.width(), 200.0);
        assert_eq!(fragment.children[1].bounds.width(), 200.0);
    }

    #[test]
    fn test_justify_content_space_between() {
        let fc = FlexFormattingContext::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Flex;
        container_style.flex_direction = FlexDirection::Row;
        container_style.justify_content = JustifyContent::SpaceBetween;

        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            Arc::new(container_style),
            FormattingContextType::Flex,
            vec![item1, item2, item3],
        );

        let constraints = LayoutConstraints::definite(500.0, 600.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // Space between: first at start, last at end, equal spacing between
        // Items: 100 + 100 + 100 = 300
        // Container: 500
        // Space: 200
        // Gaps: 2 (between 3 items)
        // Gap size: 100
        assert_eq!(fragment.children[0].bounds.x(), 0.0);
        assert_eq!(fragment.children[1].bounds.x(), 200.0); // 100 + 100 gap
        assert_eq!(fragment.children[2].bounds.x(), 400.0); // 200 + 100 width + 100 gap
    }

    #[test]
    fn test_align_items_center() {
        let fc = FlexFormattingContext::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Flex;
        container_style.flex_direction = FlexDirection::Row;
        container_style.align_items = AlignItems::Center;
        container_style.height = Some(Length::px(100.0));

        // Different height items
        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(100.0, 100.0), FormattingContextType::Block, vec![]);
        let item3 = BoxNode::new_block(create_item_style(100.0, 74.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            Arc::new(container_style),
            FormattingContextType::Flex,
            vec![item1, item2, item3],
        );

        let constraints = LayoutConstraints::definite(400.0, 200.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // Container height is 100px, items should be vertically centered
        // Taffy may round to integer pixels, so we check approximate values
        assert_eq!(fragment.children[0].bounds.y(), 25.0); // (100 - 50) / 2
        assert_eq!(fragment.children[1].bounds.y(), 0.0); // Tallest, at top
        assert_eq!(fragment.children[2].bounds.y(), 13.0); // (100 - 74) / 2 = 13
    }

    #[test]
    fn flex_align_self_overrides_parent_align_items() {
        let fc = FlexFormattingContext::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Flex;
        container_style.flex_direction = FlexDirection::Row;
        container_style.align_items = AlignItems::Center;
        container_style.height = Some(Length::px(100.0));

        let mut item_style = ComputedStyle::default();
        item_style.height = Some(Length::px(20.0));
        item_style.align_self = Some(AlignItems::FlexEnd);

        let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            Arc::new(container_style),
            FormattingContextType::Flex,
            vec![item],
        );

        let constraints = LayoutConstraints::definite(100.0, 100.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        // Parent would center to y=40; align-self:end should place it at y=80
        assert_eq!(fragment.children[0].bounds.y(), 80.0);
    }

    #[test]
    fn flex_item_aspect_ratio_sets_width_from_height() {
        let fc = FlexFormattingContext::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Flex;
        container_style.align_items = AlignItems::FlexStart;

        let mut item_style = ComputedStyle::default();
        item_style.height = Some(Length::px(40.0));
        item_style.aspect_ratio = AspectRatio::Ratio(2.0);
        let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            Arc::new(container_style),
            FormattingContextType::Flex,
            vec![item],
        );

        let constraints = LayoutConstraints::definite(200.0, 200.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        assert_eq!(fragment.children[0].bounds.width(), 80.0);
        assert_eq!(fragment.children[0].bounds.height(), 40.0);
    }

    #[test]
    fn flex_item_aspect_ratio_sets_height_from_width() {
        let fc = FlexFormattingContext::new();

        let mut container_style = ComputedStyle::default();
        container_style.display = Display::Flex;
        container_style.align_items = AlignItems::FlexStart;

        let mut item_style = ComputedStyle::default();
        item_style.width = Some(Length::px(120.0));
        item_style.aspect_ratio = AspectRatio::Ratio(3.0);
        let item = BoxNode::new_block(Arc::new(item_style), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(
            Arc::new(container_style),
            FormattingContextType::Flex,
            vec![item],
        );

        let constraints = LayoutConstraints::definite(300.0, 200.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        assert_eq!(fragment.children[0].bounds.width(), 120.0);
        assert_eq!(fragment.children[0].bounds.height(), 40.0);
    }

    #[test]
    fn test_intrinsic_sizing_max_content() {
        let fc = FlexFormattingContext::new();

        let item1 = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);
        let item2 = BoxNode::new_block(create_item_style(150.0, 50.0), FormattingContextType::Block, vec![]);

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![item1, item2]);

        let width = fc
            .compute_intrinsic_inline_size(&container, IntrinsicSizingMode::MaxContent)
            .unwrap();

        // Max-content width should be sum of children widths (row direction)
        assert_eq!(width, 250.0);
    }

    #[test]
    fn test_nested_flex() {
        let fc = FlexFormattingContext::new();

        // Inner flex container with two items
        let inner_item1 = BoxNode::new_block(create_item_style(50.0, 30.0), FormattingContextType::Block, vec![]);
        let inner_item2 = BoxNode::new_block(create_item_style(50.0, 30.0), FormattingContextType::Block, vec![]);

        let inner_container = BoxNode::new_block(
            create_flex_style(),
            FormattingContextType::Flex,
            vec![inner_item1, inner_item2],
        );

        // Outer flex container
        let outer_item = BoxNode::new_block(create_item_style(100.0, 50.0), FormattingContextType::Block, vec![]);

        let outer_container = BoxNode::new_block(
            create_flex_style(),
            FormattingContextType::Flex,
            vec![inner_container, outer_item],
        );

        let constraints = LayoutConstraints::definite(400.0, 600.0);
        let fragment = fc.layout(&outer_container, &constraints).unwrap();

        // Outer container has 2 children
        assert_eq!(fragment.children.len(), 2);

        // First child (inner container) should have 2 children
        assert_eq!(fragment.children[0].children.len(), 2);

        // Inner items should be laid out horizontally within their container
        assert_eq!(fragment.children[0].children[0].bounds.x(), 0.0);
        assert_eq!(fragment.children[0].children[1].bounds.x(), 50.0);
    }

    #[test]
    fn test_empty_flex_container() {
        let fc = FlexFormattingContext::new();

        let container = BoxNode::new_block(create_flex_style(), FormattingContextType::Flex, vec![]);

        let constraints = LayoutConstraints::definite(400.0, 600.0);
        let fragment = fc.layout(&container, &constraints).unwrap();

        assert_eq!(fragment.children.len(), 0);
    }

    #[test]
    fn test_flex_formatting_context_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<FlexFormattingContext>();
    }

    #[test]
    fn test_style_conversion_flex_direction() {
        let fc = FlexFormattingContext::new();

        assert_eq!(
            fc.flex_direction_to_taffy(FlexDirection::Row),
            taffy::style::FlexDirection::Row
        );
        assert_eq!(
            fc.flex_direction_to_taffy(FlexDirection::RowReverse),
            taffy::style::FlexDirection::RowReverse
        );
        assert_eq!(
            fc.flex_direction_to_taffy(FlexDirection::Column),
            taffy::style::FlexDirection::Column
        );
        assert_eq!(
            fc.flex_direction_to_taffy(FlexDirection::ColumnReverse),
            taffy::style::FlexDirection::ColumnReverse
        );
    }

    #[test]
    fn test_style_conversion_flex_wrap() {
        let fc = FlexFormattingContext::new();

        assert_eq!(fc.flex_wrap_to_taffy(FlexWrap::NoWrap), taffy::style::FlexWrap::NoWrap);
        assert_eq!(fc.flex_wrap_to_taffy(FlexWrap::Wrap), taffy::style::FlexWrap::Wrap);
        assert_eq!(
            fc.flex_wrap_to_taffy(FlexWrap::WrapReverse),
            taffy::style::FlexWrap::WrapReverse
        );
    }

    #[test]
    fn test_length_conversion() {
        let fc = FlexFormattingContext::new();
        let style = ComputedStyle::default();

        // Pixel values
        let len_px = Length::px(100.0);
        assert_eq!(fc.length_to_dimension(&len_px, &style), Dimension::length(100.0));

        // Percentage values
        let len_percent = Length::percent(50.0);
        assert_eq!(fc.length_to_dimension(&len_percent, &style), Dimension::percent(0.5)); // 50% = 0.5

        // Auto (None)
        assert_eq!(fc.length_option_to_dimension(None, &style), Dimension::auto());
    }
}
