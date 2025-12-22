//! Integration tests for display list optimization
//!
//! These tests verify the display list optimization module works correctly
//! in various scenarios including edge cases.

use fastrender::geometry::Point;
use fastrender::geometry::Rect;
use fastrender::paint::display_list::ClipShape;
use fastrender::paint::display_list::ResolvedFilter;
use fastrender::BlendMode;
use fastrender::BlendModeItem;
use fastrender::BorderRadii;
use fastrender::BoxShadowItem;
use fastrender::ClipItem;
use fastrender::DisplayItem;
use fastrender::DisplayList;
use fastrender::DisplayListOptimizer;
use fastrender::FillRectItem;
use fastrender::FillRoundedRectItem;
use fastrender::GradientSpread;
use fastrender::GradientStop;
use fastrender::ImageData;
use fastrender::ImageFilterQuality;
use fastrender::ImageItem;
use fastrender::LinearGradientItem;
use fastrender::OpacityItem;
use fastrender::OptimizationConfig;
use fastrender::PaintTextItem as TextItem;
use fastrender::RadialGradientItem;
use fastrender::Rgba;
use fastrender::StackingContextItem;
use fastrender::StrokeRectItem;
use fastrender::StrokeRoundedRectItem;
use fastrender::Transform2D;
use fastrender::Transform3D;
use fastrender::TransformItem;
use fastrender::style::types::BackfaceVisibility;
use fastrender::style::types::TransformStyle;
use std::sync::Arc;

// ============================================================================
// Helper Functions
// ============================================================================

fn make_fill_rect(x: f32, y: f32, w: f32, h: f32, color: Rgba) -> DisplayItem {
  DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(x, y, w, h),
    color,
  })
}

fn make_stroke_rect(x: f32, y: f32, w: f32, h: f32, color: Rgba, width: f32) -> DisplayItem {
  DisplayItem::StrokeRect(StrokeRectItem {
    rect: Rect::from_xywh(x, y, w, h),
    color,
    width,
    blend_mode: BlendMode::Normal,
  })
}

fn make_text(x: f32, y: f32, color: Rgba) -> DisplayItem {
  DisplayItem::Text(TextItem {
    origin: Point::new(x, y),
    glyphs: vec![],
    color,
    shadows: Vec::new(),
    font_size: 16.0,
    advance_width: 100.0,
    font_id: None,
    synthetic_bold: 0.0,
    synthetic_oblique: 0.0,
    emphasis: None,
    decorations: Vec::new(),
  })
}

fn make_image(x: f32, y: f32, w: f32, h: f32) -> DisplayItem {
  let pixels = vec![255u8; 4]; // 1x1 pixel
  DisplayItem::Image(ImageItem {
    dest_rect: Rect::from_xywh(x, y, w, h),
    image: Arc::new(ImageData::new_pixels(1, 1, pixels)),
    filter_quality: ImageFilterQuality::Linear,
    src_rect: None,
  })
}

fn full_viewport() -> Rect {
  Rect::from_xywh(0.0, 0.0, 1000.0, 1000.0)
}

fn small_viewport() -> Rect {
  Rect::from_xywh(0.0, 0.0, 100.0, 100.0)
}

// ============================================================================
// Display List Basic Tests
// ============================================================================

#[test]
fn test_display_list_creation() {
  let list = DisplayList::new();
  assert!(list.is_empty());
  assert_eq!(list.len(), 0);
}

#[test]
fn test_display_list_with_capacity() {
  let list = DisplayList::with_capacity(100);
  assert!(list.is_empty());
}

#[test]
fn test_display_list_push_and_len() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  assert_eq!(list.len(), 1);
  assert!(!list.is_empty());

  list.push(make_fill_rect(100.0, 0.0, 100.0, 100.0, Rgba::BLACK));
  assert_eq!(list.len(), 2);
}

#[test]
fn test_display_list_extend() {
  let mut list = DisplayList::new();
  let items = vec![
    make_fill_rect(0.0, 0.0, 50.0, 50.0, Rgba::RED),
    make_fill_rect(50.0, 0.0, 50.0, 50.0, Rgba::BLACK),
  ];
  list.extend(items);
  assert_eq!(list.len(), 2);
}

#[test]
fn test_display_list_clear() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::BLACK));
  assert_eq!(list.len(), 2);

  list.clear();
  assert!(list.is_empty());
}

#[test]
fn test_display_list_bounds() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(10.0, 10.0, 80.0, 80.0, Rgba::RED));
  list.push(make_fill_rect(50.0, 50.0, 100.0, 100.0, Rgba::BLACK));

  let bounds = list.bounds();
  assert_eq!(bounds.min_x(), 10.0);
  assert_eq!(bounds.min_y(), 10.0);
  assert_eq!(bounds.max_x(), 150.0);
  assert_eq!(bounds.max_y(), 150.0);
}

#[test]
fn test_display_list_from_items() {
  let items = vec![
    make_fill_rect(0.0, 0.0, 50.0, 50.0, Rgba::RED),
    make_fill_rect(50.0, 0.0, 50.0, 50.0, Rgba::BLACK),
  ];
  let list = DisplayList::from_items(items);
  assert_eq!(list.len(), 2);
}

// ============================================================================
// Viewport Culling Tests
// ============================================================================

#[test]
fn test_cull_items_inside_viewport() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(10.0, 10.0, 50.0, 50.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 0);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_cull_items_outside_viewport() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(200.0, 200.0, 50.0, 50.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 1);
  assert_eq!(optimized.len(), 0);
}

#[test]
fn test_cull_items_partially_in_viewport() {
  let mut list = DisplayList::new();
  // Partially inside
  list.push(make_fill_rect(80.0, 80.0, 50.0, 50.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 0);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_cull_mixed_items() {
  let mut list = DisplayList::new();
  // Inside
  list.push(make_fill_rect(10.0, 10.0, 30.0, 30.0, Rgba::RED));
  // Outside
  list.push(make_fill_rect(500.0, 500.0, 30.0, 30.0, Rgba::BLACK));
  // Partial
  list.push(make_fill_rect(
    90.0,
    10.0,
    30.0,
    30.0,
    Rgba::rgb(128, 128, 128),
  ));
  // Outside
  list.push(make_fill_rect(-200.0, -200.0, 30.0, 30.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 2);
  assert_eq!(optimized.len(), 2);
}

#[test]
fn test_cull_stroke_rect_with_width() {
  let mut list = DisplayList::new();
  // Stroke rect where the rect is outside but stroke width brings it inside
  list.push(make_stroke_rect(95.0, 50.0, 10.0, 10.0, Rgba::RED, 20.0));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  // Should be included because stroke extends into viewport
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_cull_text_items() {
  let mut list = DisplayList::new();
  list.push(make_text(10.0, 20.0, Rgba::BLACK));
  list.push(make_text(500.0, 500.0, Rgba::BLACK));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 1);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_cull_image_items() {
  let mut list = DisplayList::new();
  list.push(make_image(10.0, 10.0, 50.0, 50.0));
  list.push(make_image(500.0, 500.0, 50.0, 50.0));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 1);
  assert_eq!(optimized.len(), 1);
}

// ============================================================================
// Transparent Item Removal Tests
// ============================================================================

#[test]
fn test_remove_transparent_fill() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(10.0, 10.0, 50.0, 50.0, Rgba::TRANSPARENT));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.transparent_removed, 1);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_remove_transparent_stroke() {
  let mut list = DisplayList::new();
  list.push(make_stroke_rect(
    0.0,
    0.0,
    100.0,
    100.0,
    Rgba::TRANSPARENT,
    2.0,
  ));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.transparent_removed, 1);
  assert_eq!(optimized.len(), 0);
}

#[test]
fn test_remove_transparent_text() {
  let mut list = DisplayList::new();
  list.push(make_text(10.0, 20.0, Rgba::TRANSPARENT));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.transparent_removed, 1);
  assert_eq!(optimized.len(), 0);
}

#[test]
fn test_keep_semi_transparent() {
  let semi_transparent = Rgba::new(255, 255, 255, 0.5);
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, semi_transparent));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.transparent_removed, 0);
  assert_eq!(optimized.len(), 1);
}

// ============================================================================
// No-op Removal Tests
// ============================================================================

#[test]
fn test_remove_noop_opacity() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 1.0 }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopOpacity);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.noop_removed, 2);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_remove_noop_transform() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushTransform(TransformItem {
    transform: Transform3D::IDENTITY,
  }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopTransform);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.noop_removed, 2);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_remove_noop_blend_mode() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::Normal,
  }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopBlendMode);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.noop_removed, 2);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_keep_non_identity_transform() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushTransform(TransformItem {
    transform: Transform3D::translate(10.0, 10.0, 0.0),
  }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopTransform);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.noop_removed, 0);
  assert_eq!(optimized.len(), 3);
}

#[test]
fn test_keep_non_normal_blend_mode() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::Multiply,
  }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopBlendMode);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.noop_removed, 0);
  assert_eq!(optimized.len(), 3);
}

// ============================================================================
// Adjacent Fill Merging Tests
// ============================================================================

#[test]
fn test_merge_horizontal_fills() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(50.0, 0.0, 50.0, 100.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.merged_count, 1);
  assert_eq!(optimized.len(), 1);

  if let DisplayItem::FillRect(item) = &optimized.items()[0] {
    assert_eq!(item.rect.width(), 100.0);
    assert_eq!(item.rect.height(), 100.0);
  } else {
    panic!("Expected FillRect");
  }
}

#[test]
fn test_merge_vertical_fills() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 100.0, 50.0, Rgba::RED));
  list.push(make_fill_rect(0.0, 50.0, 100.0, 50.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.merged_count, 1);
  assert_eq!(optimized.len(), 1);

  if let DisplayItem::FillRect(item) = &optimized.items()[0] {
    assert_eq!(item.rect.width(), 100.0);
    assert_eq!(item.rect.height(), 100.0);
  } else {
    panic!("Expected FillRect");
  }
}

#[test]
fn test_no_merge_different_colors() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(50.0, 0.0, 50.0, 100.0, Rgba::BLACK));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.merged_count, 0);
  assert_eq!(optimized.len(), 2);
}

#[test]
fn test_no_merge_non_adjacent() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(60.0, 0.0, 50.0, 100.0, Rgba::RED)); // Gap of 10px

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.merged_count, 0);
  assert_eq!(optimized.len(), 2);
}

#[test]
fn test_merge_chain() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(50.0, 0.0, 50.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(100.0, 0.0, 50.0, 100.0, Rgba::RED));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  // Should merge all three into one
  assert_eq!(stats.merged_count, 2);
  assert_eq!(optimized.len(), 1);

  if let DisplayItem::FillRect(item) = &optimized.items()[0] {
    assert_eq!(item.rect.width(), 150.0);
  } else {
    panic!("Expected FillRect");
  }
}

// ============================================================================
// Optimization Configuration Tests
// ============================================================================

#[test]
fn test_disable_culling() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(500.0, 500.0, 50.0, 50.0, Rgba::RED));

  let config = OptimizationConfig {
    enable_culling: false,
    ..Default::default()
  };
  let optimizer = DisplayListOptimizer::with_config(config);
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 0);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_disable_transparent_removal() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::TRANSPARENT));

  let config = OptimizationConfig {
    enable_transparent_removal: false,
    ..Default::default()
  };
  let optimizer = DisplayListOptimizer::with_config(config);
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.transparent_removed, 0);
  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_disable_fill_merging() {
  let mut list = DisplayList::new();
  list.push(make_fill_rect(0.0, 0.0, 50.0, 100.0, Rgba::RED));
  list.push(make_fill_rect(50.0, 0.0, 50.0, 100.0, Rgba::RED));

  let config = OptimizationConfig {
    enable_fill_merging: false,
    ..Default::default()
  };
  let optimizer = DisplayListOptimizer::with_config(config);
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.merged_count, 0);
  assert_eq!(optimized.len(), 2);
}

#[test]
fn test_disable_noop_removal() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 1.0 }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopOpacity);

  let config = OptimizationConfig {
    enable_noop_removal: false,
    ..Default::default()
  };
  let optimizer = DisplayListOptimizer::with_config(config);
  let (optimized, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.noop_removed, 0);
  assert_eq!(optimized.len(), 3);
}

// ============================================================================
// Complex Item Tests
// ============================================================================

#[test]
fn test_box_shadow_culling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(10.0, 10.0, 50.0, 50.0),
    radii: BorderRadii::ZERO,
    offset: Point::new(5.0, 5.0),
    blur_radius: 10.0,
    spread_radius: 5.0,
    color: Rgba::new(0, 0, 0, 0.5),
    inset: false,
  }));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_box_shadow_outside_viewport() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(500.0, 500.0, 50.0, 50.0),
    radii: BorderRadii::ZERO,
    offset: Point::ZERO,
    blur_radius: 10.0,
    spread_radius: 0.0,
    color: Rgba::new(0, 0, 0, 0.5),
    inset: false,
  }));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(stats.culled_count, 1);
  assert_eq!(optimized.len(), 0);
}

#[test]
fn test_box_shadow_offset_into_view_kept() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::BoxShadow(BoxShadowItem {
    rect: Rect::from_xywh(-300.0, 0.0, 50.0, 50.0),
    radii: BorderRadii::ZERO,
    offset: Point::new(320.0, 0.0),
    blur_radius: 5.0,
    spread_radius: 0.0,
    color: Rgba::new(0, 0, 0, 0.5),
    inset: false,
  }));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(
    optimized.len(),
    1,
    "shadow drawn into viewport should remain"
  );
}

#[test]
fn test_linear_gradient_culling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::LinearGradient(LinearGradientItem {
    rect: Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
    start: Point::new(0.0, 0.0),
    end: Point::new(80.0, 80.0),
    spread: GradientSpread::Pad,
    stops: vec![
      GradientStop {
        position: 0.0,
        color: Rgba::RED,
      },
      GradientStop {
        position: 1.0,
        color: Rgba::BLACK,
      },
    ],
  }));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_radial_gradient_culling() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::RadialGradient(RadialGradientItem {
    rect: Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
    center: Point::new(40.0, 40.0),
    radii: Point::new(40.0, 40.0),
    spread: GradientSpread::Pad,
    stops: vec![
      GradientStop {
        position: 0.0,
        color: Rgba::RED,
      },
      GradientStop {
        position: 1.0,
        color: Rgba::BLACK,
      },
    ],
  }));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(optimized.len(), 1);
}

#[test]
fn test_rounded_rect_items() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
    rect: Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
    color: Rgba::RED,
    radii: BorderRadii::uniform(10.0),
  }));
  list.push(DisplayItem::StrokeRoundedRect(StrokeRoundedRectItem {
    rect: Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
    color: Rgba::BLACK,
    width: 2.0,
    radii: BorderRadii::uniform(10.0),
  }));

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(optimized.len(), 2);
}

// ============================================================================
// Clip Region Tests
// ============================================================================

#[test]
fn test_clip_inside_viewport() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: Rect::from_xywh(10.0, 10.0, 80.0, 80.0),
      radii: None,
    },
  }));
  list.push(make_fill_rect(20.0, 20.0, 60.0, 60.0, Rgba::RED));
  list.push(DisplayItem::PopClip);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(optimized.len(), 3);
}

#[test]
fn test_clip_outside_viewport_culls_children() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: Rect::from_xywh(500.0, 500.0, 80.0, 80.0),
      radii: None,
    },
  }));
  list.push(make_fill_rect(510.0, 510.0, 60.0, 60.0, Rgba::RED));
  list.push(DisplayItem::PopClip);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  // Clip is outside viewport, so children should be culled
  // Only the balanced stack operations remain
  assert!(optimized.len() <= 2);
}

#[test]
fn transform_keeps_clipped_content_from_being_culled() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: Rect::from_xywh(500.0, 500.0, 50.0, 50.0),
      radii: None,
    },
  }));
  // Transform could reposition content relative to the viewport, so the optimizer should not drop it.
  list.push(DisplayItem::PushTransform(TransformItem {
    transform: Transform3D::translate(-450.0, -450.0, 0.0),
  }));
  list.push(make_fill_rect(500.0, 500.0, 20.0, 20.0, Rgba::RED));
  list.push(DisplayItem::PopTransform);
  list.push(DisplayItem::PopClip);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  // Clip would normally cull, but the active transform means we conservatively keep the content.
  assert_eq!(optimized.len(), 5);
}

// ============================================================================
// Transform Tests
// ============================================================================

#[test]
fn test_transform_identity_check() {
  let identity = Transform2D::IDENTITY;
  assert!(identity.is_identity());

  let translate = Transform2D::translate(10.0, 10.0);
  assert!(!translate.is_identity());

  let scale = Transform2D::scale(2.0, 2.0);
  assert!(!scale.is_identity());
}

#[test]
fn test_transform_point() {
  let translate = Transform2D::translate(10.0, 20.0);
  let p = Point::new(5.0, 5.0);
  let result = translate.transform_point(p);
  assert_eq!(result.x, 15.0);
  assert_eq!(result.y, 25.0);
}

#[test]
fn test_transform_scale_point() {
  let scale = Transform2D::scale(2.0, 3.0);
  let p = Point::new(10.0, 10.0);
  let result = scale.transform_point(p);
  assert_eq!(result.x, 20.0);
  assert_eq!(result.y, 30.0);
}

#[test]
fn test_transform_rotate() {
  let rotate = Transform2D::rotate(std::f32::consts::PI / 2.0); // 90 degrees
  let p = Point::new(1.0, 0.0);
  let result = rotate.transform_point(p);
  assert!((result.x - 0.0).abs() < 0.001);
  assert!((result.y - 1.0).abs() < 0.001);
}

#[test]
fn test_transform_multiply() {
  let t1 = Transform2D::translate(10.0, 0.0);
  let t2 = Transform2D::scale(2.0, 1.0);
  let combined = t1.multiply(&t2);

  let p = Point::new(5.0, 0.0);
  let result = combined.transform_point(p);
  // Scale first: 5 * 2 = 10, then translate: 10 + 10 = 20
  assert_eq!(result.x, 20.0);
}

#[test]
fn test_transform_rect() {
  let translate = Transform2D::translate(10.0, 20.0);
  let rect = Rect::from_xywh(0.0, 0.0, 100.0, 50.0);
  let result = translate.transform_rect(rect);

  assert_eq!(result.min_x(), 10.0);
  assert_eq!(result.min_y(), 20.0);
  assert_eq!(result.width(), 100.0);
  assert_eq!(result.height(), 50.0);
}

// ============================================================================
// Border Radii Tests
// ============================================================================

#[test]
fn test_border_radii_zero() {
  let radii = BorderRadii::ZERO;
  assert!(!radii.has_radius());
  assert!(radii.is_uniform());
}

#[test]
fn test_border_radii_uniform() {
  let radii = BorderRadii::uniform(10.0);
  assert!(radii.has_radius());
  assert!(radii.is_uniform());
  assert_eq!(radii.top_left, 10.0);
  assert_eq!(radii.bottom_right, 10.0);
}

#[test]
fn test_border_radii_non_uniform() {
  let radii = BorderRadii::new(10.0, 20.0, 15.0, 5.0);
  assert!(radii.has_radius());
  assert!(!radii.is_uniform());
}

// ============================================================================
// Optimization Statistics Tests
// ============================================================================

#[test]
fn test_stats_reduction_percentage() {
  let mut list = DisplayList::new();
  for _ in 0..10 {
    list.push(make_fill_rect(0.0, 0.0, 50.0, 50.0, Rgba::TRANSPARENT));
  }

  let optimizer = DisplayListOptimizer::new();
  let (_, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.original_count, 10);
  assert_eq!(stats.final_count, 0);
  assert_eq!(stats.reduction_percentage(), 100.0);
}

#[test]
fn test_stats_zero_items() {
  let list = DisplayList::new();
  let optimizer = DisplayListOptimizer::new();
  let (_, stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(stats.original_count, 0);
  assert_eq!(stats.final_count, 0);
  assert_eq!(stats.reduction_percentage(), 0.0);
}

// ============================================================================
// Stacking Context Tests
// ============================================================================

#[test]
fn test_stacking_context_preserved() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 1,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
  }));
  list.push(make_fill_rect(0.0, 0.0, 100.0, 100.0, Rgba::RED));
  list.push(DisplayItem::PopStackingContext);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, full_viewport());

  assert_eq!(optimized.len(), 3);
}

#[test]
fn stacking_context_filters_expand_cull_bounds() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(-10.0, 0.0, 5.0, 5.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::DropShadow {
      offset_x: 20.0,
      offset_y: 0.0,
      blur_radius: 0.0,
      spread: 0.0,
      color: Rgba::BLACK,
    }],
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
  }));
  list.push(make_fill_rect(-10.0, 0.0, 5.0, 5.0, Rgba::RED));
  list.push(DisplayItem::PopStackingContext);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  // Drop shadow spills into the viewport, so the context and its content should be preserved.
  assert_eq!(optimized.len(), 3);
}

#[test]
fn offscreen_filtered_stacking_context_is_culled() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(500.0, 500.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::DropShadow {
      offset_x: 5.0,
      offset_y: 5.0,
      blur_radius: 0.0,
      spread: 0.0,
      color: Rgba::BLACK,
    }],
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
  }));
  list.push(make_fill_rect(500.0, 500.0, 10.0, 10.0, Rgba::RED));
  list.push(DisplayItem::PopStackingContext);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert!(optimized.is_empty());
}

#[test]
fn transforms_keep_children_from_being_culled() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushTransform(TransformItem {
    transform: Transform3D::translate(150.0, 0.0, 0.0),
  }));
  list.push(make_fill_rect(-120.0, 0.0, 10.0, 10.0, Rgba::RED));
  list.push(DisplayItem::PopTransform);

  let optimizer = DisplayListOptimizer::new();
  let (optimized, _stats) = optimizer.optimize(list, small_viewport());

  assert_eq!(optimized.len(), 3);
}

// ============================================================================
// Blend Mode Tests
// ============================================================================

#[test]
fn test_blend_mode_default() {
  assert_eq!(BlendMode::default(), BlendMode::Normal);
}
