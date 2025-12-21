//! Integration tests for FormattingContextFactory
//!
//! These tests verify that all formatting contexts are properly integrated
//! and work together through the factory dispatch mechanism.

#[allow(unused_imports)]
use fastrender::geometry::Rect;
use fastrender::style::display::Display;
use fastrender::style::float::Float;
use fastrender::style::values::Length;
use fastrender::ComputedStyle;
use fastrender::FormattingContextFactory;
#[allow(unused_imports)]
use fastrender::{BoxNode, FormattingContextType, FragmentNode};
use fastrender::{FormattingContext, IntrinsicSizingMode, LayoutConstraints};
use std::sync::Arc;

// =============================================================================
// Helper Functions
// =============================================================================

fn default_style() -> Arc<ComputedStyle> {
    Arc::new(ComputedStyle::default())
}

fn create_block_box(children: Vec<BoxNode>) -> BoxNode {
    BoxNode::new_block(default_style(), FormattingContextType::Block, children)
}

fn create_flex_box(children: Vec<BoxNode>) -> BoxNode {
    BoxNode::new_block(default_style(), FormattingContextType::Flex, children)
}

fn create_grid_box(children: Vec<BoxNode>) -> BoxNode {
    BoxNode::new_block(default_style(), FormattingContextType::Grid, children)
}

fn create_table_box(children: Vec<BoxNode>) -> BoxNode {
    BoxNode::new_block(default_style(), FormattingContextType::Table, children)
}

fn create_inline_fc_box(children: Vec<BoxNode>) -> BoxNode {
    BoxNode::new_block(default_style(), FormattingContextType::Inline, children)
}

fn create_text_box(text: &str) -> BoxNode {
    BoxNode::new_text(default_style(), text.to_string())
}

fn standard_constraints() -> LayoutConstraints {
    LayoutConstraints::definite(800.0, 600.0)
}

fn wide_constraints() -> LayoutConstraints {
    LayoutConstraints::definite(1920.0, 1080.0)
}

fn narrow_constraints() -> LayoutConstraints {
    LayoutConstraints::definite(320.0, 480.0)
}

// =============================================================================
// Factory Creation Tests
// =============================================================================

#[test]
fn test_factory_creates_all_fc_types() {
    let factory = FormattingContextFactory::new();

    // Verify all types can be created
    let _block = factory.create(FormattingContextType::Block);
    let _inline = factory.create(FormattingContextType::Inline);
    let _flex = factory.create(FormattingContextType::Flex);
    let _grid = factory.create(FormattingContextType::Grid);
    let _table = factory.create(FormattingContextType::Table);
}

#[test]
fn test_factory_create_for_box_dispatches_correctly() {
    let factory = FormattingContextFactory::new();

    // Block box
    let block_box = create_block_box(vec![]);
    let block_fc = factory.create_for_box(&block_box).unwrap();
    assert!(block_fc.layout(&block_box, &standard_constraints()).is_ok());

    // Flex box
    let flex_box = create_flex_box(vec![]);
    let flex_fc = factory.create_for_box(&flex_box).unwrap();
    assert!(flex_fc.layout(&flex_box, &standard_constraints()).is_ok());

    // Grid box
    let grid_box = create_grid_box(vec![]);
    let grid_fc = factory.create_for_box(&grid_box).unwrap();
    assert!(grid_fc.layout(&grid_box, &standard_constraints()).is_ok());

    // Table box
    let table_box = create_table_box(vec![]);
    let table_fc = factory.create_for_box(&table_box).unwrap();
    assert!(table_fc.layout(&table_box, &standard_constraints()).is_ok());
}

#[test]
fn test_factory_rejects_inline_boxes() {
    let factory = FormattingContextFactory::new();
    let inline_box = BoxNode::new_inline(default_style(), vec![]);

    let result = factory.create_for_box(&inline_box);
    assert!(result.is_err());
}

// =============================================================================
// Block Formatting Context Integration
// =============================================================================

#[test]
fn test_block_fc_empty_layout() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Block);
    let box_node = create_block_box(vec![]);

    let fragment = fc.layout(&box_node, &standard_constraints()).unwrap();

    // Empty block should fill available width
    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.bounds.height(), 0.0);
}

#[test]
fn test_block_fc_with_children() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Block);

    let child1 = create_block_box(vec![]);
    let child2 = create_block_box(vec![]);
    let parent = create_block_box(vec![child1, child2]);

    let fragment = fc.layout(&parent, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    // Children stack vertically
    assert_eq!(fragment.children.len(), 2);
}

#[test]
fn test_block_fc_intrinsic_sizing() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Block);
    let box_node = create_block_box(vec![]);

    let min_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
        .unwrap();
    let max_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
        .unwrap();

    assert!(min_content >= 0.0);
    assert!(max_content >= min_content);
}

// =============================================================================
// Inline Formatting Context Integration
// =============================================================================

#[test]
fn test_inline_fc_empty_layout() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Inline);
    let box_node = create_inline_fc_box(vec![]);

    let fragment = fc.layout(&box_node, &standard_constraints()).unwrap();

    // Empty inline FC should have zero height (no lines)
    assert!(fragment.bounds.height() >= 0.0);
}

#[test]
fn test_inline_fc_with_text() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Inline);

    let text_box = create_text_box("Hello World");
    let parent = create_inline_fc_box(vec![text_box]);

    let fragment = fc.layout(&parent, &standard_constraints()).unwrap();

    // Should have some content height
    assert!(fragment.bounds.width() >= 0.0);
}

#[test]
fn test_inline_fc_intrinsic_sizing() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Inline);

    let text_box = create_text_box("Hello World");
    let box_node = create_inline_fc_box(vec![text_box]);

    let min_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
        .unwrap();
    let max_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
        .unwrap();

    // Max content >= min content
    assert!(max_content >= min_content);
}

// =============================================================================
// Flex Formatting Context Integration
// =============================================================================

#[test]
fn test_flex_fc_empty_layout() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Flex);
    let box_node = create_flex_box(vec![]);

    let fragment = fc.layout(&box_node, &standard_constraints()).unwrap();

    // Flex container fills available width
    assert_eq!(fragment.bounds.width(), 800.0);
}

#[test]
fn test_flex_fc_with_children() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Flex);

    let child1 = create_block_box(vec![]);
    let child2 = create_block_box(vec![]);
    let parent = create_flex_box(vec![child1, child2]);

    let fragment = fc.layout(&parent, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.children.len(), 2);
}

#[test]
fn test_flex_fc_intrinsic_sizing() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Flex);
    let box_node = create_flex_box(vec![]);

    let min_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
        .unwrap();
    let max_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
        .unwrap();

    assert!(min_content >= 0.0);
    assert!(max_content >= 0.0);
}

// =============================================================================
// Grid Formatting Context Integration
// =============================================================================

#[test]
fn test_grid_fc_empty_layout() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Grid);
    let box_node = create_grid_box(vec![]);

    let fragment = fc.layout(&box_node, &standard_constraints()).unwrap();

    // Grid container fills available width
    assert_eq!(fragment.bounds.width(), 800.0);
}

#[test]
fn test_grid_fc_with_children() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Grid);

    let child1 = create_block_box(vec![]);
    let child2 = create_block_box(vec![]);
    let parent = create_grid_box(vec![child1, child2]);

    let fragment = fc.layout(&parent, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.children.len(), 2);
}

#[test]
fn test_grid_fc_intrinsic_sizing() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Grid);
    let box_node = create_grid_box(vec![]);

    let min_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
        .unwrap();
    let max_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
        .unwrap();

    assert!(min_content >= 0.0);
    assert!(max_content >= 0.0);
}

// =============================================================================
// Table Formatting Context Integration
// =============================================================================

#[test]
fn test_table_fc_empty_layout() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Table);
    let box_node = create_table_box(vec![]);

    let fragment = fc.layout(&box_node, &standard_constraints()).unwrap();

    // Empty table should have valid bounds
    assert!(fragment.bounds.width() >= 0.0);
    assert!(fragment.bounds.height() >= 0.0);
}

#[test]
fn test_table_fc_intrinsic_sizing() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Table);
    let box_node = create_table_box(vec![]);

    let min_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
        .unwrap();
    let max_content = fc
        .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
        .unwrap();

    assert!(min_content >= 0.0);
    assert!(max_content >= 0.0);
}

// =============================================================================
// Cross-FC Integration Tests
// =============================================================================

#[test]
fn test_block_containing_flex() {
    let factory = FormattingContextFactory::new();
    let block_fc = factory.create(FormattingContextType::Block);

    let flex_child = create_flex_box(vec![]);
    let block_parent = create_block_box(vec![flex_child]);

    let fragment = block_fc.layout(&block_parent, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.children.len(), 1);
}

#[test]
fn test_flex_containing_block() {
    let factory = FormattingContextFactory::new();
    let flex_fc = factory.create(FormattingContextType::Flex);

    let block_child = create_block_box(vec![]);
    let flex_parent = create_flex_box(vec![block_child]);

    let fragment = flex_fc.layout(&flex_parent, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.children.len(), 1);
}

#[test]
fn test_grid_containing_flex() {
    let factory = FormattingContextFactory::new();
    let grid_fc = factory.create(FormattingContextType::Grid);

    let flex_child = create_flex_box(vec![]);
    let grid_parent = create_grid_box(vec![flex_child]);

    let fragment = grid_fc.layout(&grid_parent, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.children.len(), 1);
}

#[test]
fn test_deeply_nested_fcs() {
    let factory = FormattingContextFactory::new();
    let block_fc = factory.create(FormattingContextType::Block);

    // Create: Block > Flex > Grid > Block
    let inner_block = create_block_box(vec![]);
    let grid = create_grid_box(vec![inner_block]);
    let flex = create_flex_box(vec![grid]);
    let outer_block = create_block_box(vec![flex]);

    let fragment = block_fc.layout(&outer_block, &standard_constraints()).unwrap();

    assert_eq!(fragment.bounds.width(), 800.0);
    assert_eq!(fragment.children.len(), 1); // flex
}

// =============================================================================
// Constraint Handling Tests
// =============================================================================

#[test]
fn test_all_fcs_handle_wide_constraints() {
    let factory = FormattingContextFactory::new();
    let constraints = wide_constraints();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let result = fc.layout(&box_node, &constraints);
        assert!(result.is_ok(), "FC {:?} failed with wide constraints", fc_type);
    }
}

#[test]
fn test_all_fcs_handle_narrow_constraints() {
    let factory = FormattingContextFactory::new();
    let constraints = narrow_constraints();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let result = fc.layout(&box_node, &constraints);
        assert!(result.is_ok(), "FC {:?} failed with narrow constraints", fc_type);
    }
}

#[test]
fn test_all_fcs_handle_indefinite_constraints() {
    let factory = FormattingContextFactory::new();
    let constraints = LayoutConstraints::indefinite();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let result = fc.layout(&box_node, &constraints);
        assert!(result.is_ok(), "FC {:?} failed with indefinite constraints", fc_type);
    }
}

// =============================================================================
// Factory Reusability Tests
// =============================================================================

#[test]
fn test_factory_can_be_reused() {
    let factory = FormattingContextFactory::new();

    for _ in 0..100 {
        for &fc_type in factory.supported_types() {
            let fc = factory.create(fc_type);
            let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);
            let result = fc.layout(&box_node, &standard_constraints());
            assert!(result.is_ok());
        }
    }
}

#[test]
fn test_created_fc_can_be_reused() {
    let factory = FormattingContextFactory::new();
    let fc = factory.create(FormattingContextType::Block);

    for i in 0..10 {
        let box_node = create_block_box(vec![]);
        let result = fc.layout(&box_node, &standard_constraints());
        assert!(result.is_ok(), "Layout {} failed", i);
    }
}

// =============================================================================
// Thread Safety Tests
// =============================================================================

#[test]
fn test_factory_is_thread_safe() {
    use std::sync::Arc;
    use std::thread;

    let factory = Arc::new(FormattingContextFactory::new());
    let mut handles = vec![];

    for _ in 0..4 {
        let factory_clone = Arc::clone(&factory);
        let handle = thread::spawn(move || {
            for &fc_type in factory_clone.supported_types() {
                let fc = factory_clone.create(fc_type);
                let box_node = BoxNode::new_block(Arc::new(ComputedStyle::default()), fc_type, vec![]);
                let constraints = LayoutConstraints::definite(800.0, 600.0);
                let result = fc.layout(&box_node, &constraints);
                assert!(result.is_ok());
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn test_created_fcs_can_be_shared_across_threads() {
    use std::sync::Arc;
    use std::thread;

    let factory = FormattingContextFactory::new();
    let fc: Arc<dyn FormattingContext> = Arc::from(factory.create(FormattingContextType::Block));

    let mut handles = vec![];

    for _ in 0..4 {
        let fc_clone = Arc::clone(&fc);
        let handle = thread::spawn(move || {
            for _ in 0..10 {
                let box_node =
                    BoxNode::new_block(Arc::new(ComputedStyle::default()), FormattingContextType::Block, vec![]);
                let constraints = LayoutConstraints::definite(800.0, 600.0);
                let result = fc_clone.layout(&box_node, &constraints);
                assert!(result.is_ok());
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }
}

// =============================================================================
// Intrinsic Sizing Mode Tests
// =============================================================================

#[test]
fn test_all_fcs_support_min_content() {
    let factory = FormattingContextFactory::new();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let result = fc.compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent);
        assert!(result.is_ok(), "FC {:?} failed min-content sizing", fc_type);
        assert!(result.unwrap() >= 0.0);
    }
}

#[test]
fn test_all_fcs_support_max_content() {
    let factory = FormattingContextFactory::new();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let result = fc.compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent);
        assert!(result.is_ok(), "FC {:?} failed max-content sizing", fc_type);
        assert!(result.unwrap() >= 0.0);
    }
}

#[test]
fn test_max_content_gte_min_content() {
    let factory = FormattingContextFactory::new();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let min = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MinContent)
            .unwrap();
        let max = fc
            .compute_intrinsic_inline_size(&box_node, IntrinsicSizingMode::MaxContent)
            .unwrap();

        assert!(
            max >= min,
            "FC {:?}: max-content ({}) < min-content ({})",
            fc_type,
            max,
            min
        );
    }
}

#[test]
fn block_intrinsic_width_respects_min_width() {
    let factory = FormattingContextFactory::new();
    let mut style = ComputedStyle::default();
    style.min_width = Some(fastrender::style::values::Length::px(50.0));
    let block = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![]);
    let width = factory
        .create(FormattingContextType::Block)
        .compute_intrinsic_inline_size(&block, IntrinsicSizingMode::MinContent)
        .expect("intrinsic width");
    assert!(
        width >= 50.0,
        "min-width should clamp intrinsic width to at least 50px, got {}",
        width
    );
}

#[test]
fn block_intrinsic_width_respects_max_width() {
    let factory = FormattingContextFactory::new();
    let mut style = ComputedStyle::default();
    style.max_width = Some(fastrender::style::values::Length::px(5.0));
    let text = create_text_box("this text should exceed five pixels");
    let block = BoxNode::new_block(Arc::new(style), FormattingContextType::Block, vec![text]);
    let width = factory
        .create(FormattingContextType::Block)
        .compute_intrinsic_inline_size(&block, IntrinsicSizingMode::MaxContent)
        .expect("intrinsic width");
    assert!(
        width <= 5.01,
        "max-width should clamp intrinsic width to 5px, got {}",
        width
    );
}

#[test]
fn block_intrinsic_width_ignores_floats() {
    let factory = FormattingContextFactory::new();

    let mut float_style = ComputedStyle::default();
    float_style.display = Display::Block;
    float_style.float = Float::Left;
    float_style.width = Some(Length::px(200.0));
    float_style.height = Some(Length::px(10.0));
    let float_child = BoxNode::new_block(Arc::new(float_style), FormattingContextType::Block, vec![]);

    let parent = BoxNode::new_block(default_style(), FormattingContextType::Block, vec![float_child]);
    let block_fc = factory.create(FormattingContextType::Block);

    let min = block_fc
        .compute_intrinsic_inline_size(&parent, IntrinsicSizingMode::MinContent)
        .expect("min-content width");
    let max = block_fc
        .compute_intrinsic_inline_size(&parent, IntrinsicSizingMode::MaxContent)
        .expect("max-content width");

    assert!(
        min <= 0.01,
        "floats are out-of-flow and should not raise min-content width; got {}",
        min
    );
    assert!(
        max <= 0.01,
        "floats are out-of-flow and should not raise max-content width; got {}",
        max
    );
}

// =============================================================================
// Fragment Structure Tests
// =============================================================================

#[test]
fn test_fragments_have_valid_bounds() {
    let factory = FormattingContextFactory::new();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        let box_node = BoxNode::new_block(default_style(), fc_type, vec![]);

        let fragment = fc.layout(&box_node, &standard_constraints()).unwrap();

        assert!(
            fragment.bounds.width() >= 0.0,
            "FC {:?} produced negative width",
            fc_type
        );
        assert!(
            fragment.bounds.height() >= 0.0,
            "FC {:?} produced negative height",
            fc_type
        );
    }
}

#[test]
fn test_child_fragments_are_preserved() {
    let factory = FormattingContextFactory::new();

    for fc_type in [
        FormattingContextType::Block,
        FormattingContextType::Flex,
        FormattingContextType::Grid,
    ] {
        let fc = factory.create(fc_type);
        let child1 = create_block_box(vec![]);
        let child2 = create_block_box(vec![]);
        let parent = BoxNode::new_block(default_style(), fc_type, vec![child1, child2]);

        let fragment = fc.layout(&parent, &standard_constraints()).unwrap();

        assert_eq!(
            fragment.children.len(),
            2,
            "FC {:?} didn't preserve children count",
            fc_type
        );
    }
}

// =============================================================================
// API Convenience Tests
// =============================================================================

#[test]
fn test_supported_types_returns_all_five() {
    let factory = FormattingContextFactory::new();
    let types = factory.supported_types();

    assert_eq!(types.len(), 5);
}

#[test]
fn test_is_supported_returns_true_for_all_types() {
    let factory = FormattingContextFactory::new();

    assert!(factory.is_supported(FormattingContextType::Block));
    assert!(factory.is_supported(FormattingContextType::Inline));
    assert!(factory.is_supported(FormattingContextType::Flex));
    assert!(factory.is_supported(FormattingContextType::Grid));
    assert!(factory.is_supported(FormattingContextType::Table));
}

#[test]
fn test_default_factory_works() {
    let factory = FormattingContextFactory::default();

    for &fc_type in factory.supported_types() {
        let fc = factory.create(fc_type);
        assert!(fc
            .layout(
                &BoxNode::new_block(default_style(), fc_type, vec![]),
                &standard_constraints()
            )
            .is_ok());
    }
}
