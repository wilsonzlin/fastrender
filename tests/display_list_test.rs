//! Integration tests for display list types
//!
//! These tests verify the display list types work correctly for the paint system.
//! Unit tests are in the display_list module itself; these are integration tests.

use fastrender::css::types::{BoxShadow, ColorStop};
use fastrender::geometry::{Point, Rect};
use fastrender::paint::display_list::ClipShape;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::css::types::{Declaration, PropertyValue};
use fastrender::style::properties::{apply_declaration, with_image_set_dpr};
use fastrender::style::types::{
    BackgroundAttachment, BackgroundBox, BackgroundImage, BackgroundLayer, BackgroundRepeat, BorderStyle, Containment,
    TextDecorationLine, WritingMode,
};
use fastrender::style::values::Length;
use fastrender::text::pipeline::{Direction, GlyphPosition, RunRotation, ShapedRun};
use fastrender::tree::box_tree::ReplacedType;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::{
    BlendMode, BorderRadii, BoxShadowItem, ClipItem, DisplayItem, DisplayList, FillRectItem, FillRoundedRectItem,
    FontContext, GlyphInstance, GradientSpread, GradientStop, ImageData, ImageFilterQuality, ImageItem,
    LinearGradientItem, OpacityItem, PaintTextItem as TextItem, RadialGradientItem, StrokeRectItem,
    StrokeRoundedRectItem, Transform2D, TransformItem,
};
use fastrender::{Color, ComputedStyle, Rgba};
use std::sync::Arc;

// ============================================================================
// DisplayList Construction Tests
// ============================================================================

#[test]
fn test_display_list_construction() {
    let list = DisplayList::new();
    assert!(list.is_empty());
    assert_eq!(list.len(), 0);
}

#[test]
fn test_display_list_with_capacity() {
    let list = DisplayList::with_capacity(100);
    assert!(list.is_empty());
    // Capacity is an optimization hint, not guaranteed behavior
}

#[test]
fn test_display_list_from_items() {
    let items = vec![
        DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
            color: Rgba::RED,
        }),
        DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
            color: Rgba::BLUE,
        }),
    ];

    let list = DisplayList::from_items(items);
    assert_eq!(list.len(), 2);
}

#[test]
fn fragment_background_emits_fill() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.border_top_width = fastrender::style::values::Length::px(0.0);
    style.border_right_width = fastrender::style::values::Length::px(0.0);
    style.border_bottom_width = fastrender::style::values::Length::px(0.0);
    style.border_left_width = fastrender::style::values::Length::px(0.0);
    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 10.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    assert!(!list.is_empty());
    match &list.items()[0] {
        DisplayItem::FillRect(item) => {
            assert_eq!(item.rect.width(), 20.0);
            assert_eq!(item.color, Rgba::RED);
        }
        _ => panic!("expected background fill"),
    }
}

#[test]
fn fragment_uniform_border_emits_stroke() {
    let mut style = fastrender::ComputedStyle::default();
    style.border_top_width = fastrender::style::values::Length::px(2.0);
    style.border_right_width = fastrender::style::values::Length::px(2.0);
    style.border_bottom_width = fastrender::style::values::Length::px(2.0);
    style.border_left_width = fastrender::style::values::Length::px(2.0);
    style.border_top_style = fastrender::style::types::BorderStyle::Solid;
    style.border_right_style = fastrender::style::types::BorderStyle::Solid;
    style.border_bottom_style = fastrender::style::types::BorderStyle::Solid;
    style.border_left_style = fastrender::style::types::BorderStyle::Solid;
    style.border_top_color = Rgba::BLUE;
    style.border_right_color = Rgba::BLUE;
    style.border_bottom_color = Rgba::BLUE;
    style.border_left_color = Rgba::BLUE;

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 10.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    assert!(list.items().iter().any(|i| matches!(
        i,
        DisplayItem::StrokeRect(_) | DisplayItem::StrokeRoundedRect(_) | DisplayItem::Border(_)
    )));
}

#[test]
fn fragment_mixed_borders_emit_border_item() {
    let mut style = fastrender::ComputedStyle::default();
    style.border_top_width = fastrender::style::values::Length::px(1.0);
    style.border_right_width = fastrender::style::values::Length::px(2.0);
    style.border_bottom_width = fastrender::style::values::Length::px(0.0);
    style.border_left_width = fastrender::style::values::Length::px(3.0);
    style.border_top_style = BorderStyle::Dotted;
    style.border_right_style = BorderStyle::Dashed;
    style.border_bottom_style = BorderStyle::Solid;
    style.border_left_style = BorderStyle::Double;
    style.border_top_color = Rgba::RED;
    style.border_right_color = Rgba::BLUE;
    style.border_left_color = Rgba::GREEN;
    style.border_bottom_color = Rgba::TRANSPARENT;

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 10.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    let border = list
        .items()
        .iter()
        .find_map(|i| if let DisplayItem::Border(b) = i { Some(b) } else { None });

    let border = border.expect("expected border item");
    assert_eq!(border.top.style, BorderStyle::Dotted);
    assert_eq!(border.right.style, BorderStyle::Dashed);
    assert_eq!(border.left.style, BorderStyle::Double);
    assert_eq!(border.top.width, 1.0);
    assert_eq!(border.right.width, 2.0);
    assert_eq!(border.left.width, 3.0);
}

#[test]
fn background_image_set_chooses_best_density_for_display_list() {
    // Two inline SVG candidates with different intrinsic sizes; DPR=2 should pick the larger one.
    let low = "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='1' height='1'></svg>";
    let high = "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'></svg>";

    let mut style = ComputedStyle::default();
    with_image_set_dpr(2.0, || {
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Keyword(format!("image-set(url(\"{}\") 1x, url(\"{}\") 2x)", low, high)),
                raw_value: String::new(),
                important: false,
            },
            &ComputedStyle::default(),
            16.0,
            16.0,
        );
    });

    let fragment = FragmentNode::new_block_styled(
        Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
        Vec::new(),
        Arc::new(style),
    );

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new()
        .with_device_pixel_ratio(2.0)
        .build(&fragment);

    let image_item = list
        .items()
        .iter()
        .find_map(|item| match item {
            DisplayItem::Image(img) => Some(img),
            _ => None,
        })
        .expect("background image to emit an Image item");

    assert_eq!(image_item.image.width, 4);
    assert_eq!(image_item.image.height, 4);
}

#[test]
fn background_attachment_fixed_anchors_to_viewport_in_display_list() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 90.0,
            stops: vec![
                ColorStop {
                    color: Color::Rgba(Rgba::RED),
                    position: Some(0.0),
                },
                ColorStop {
                    color: Color::Rgba(Rgba::BLUE),
                    position: Some(1.0),
                },
            ],
        }),
        attachment: BackgroundAttachment::Fixed,
        ..BackgroundLayer::default()
    }]);
    let style = Arc::new(style);

    let first = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], style.clone());
    let second = FragmentNode::new_block_styled(Rect::from_xywh(1.0, 0.0, 1.0, 1.0), vec![], style.clone());
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 2.0, 1.0), vec![first, second]);

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new()
        .with_viewport_size(2.0, 1.0)
        .build(&root);

    let pixmap = DisplayListRenderer::new(2, 1, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(&list)
        .unwrap();
    let pixel = |x: u32, y: u32| {
        let px = pixmap.pixel(x, y).expect("pixel inside viewport");
        (px.red(), px.green(), px.blue(), px.alpha())
    };

    assert_ne!(
        pixel(0, 0),
        pixel(1, 0),
        "fixed backgrounds should stay anchored to the viewport, changing colors across global x positions"
    );
}

#[test]
fn fragment_background_gradient_emits_linear_gradient() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![
                ColorStop {
                    color: Color::Rgba(Rgba::RED),
                    position: Some(0.0),
                },
                ColorStop {
                    color: Color::Rgba(Rgba::BLUE),
                    position: Some(1.0),
                },
            ],
        }),
        repeat: BackgroundRepeat::no_repeat(),
        ..Default::default()
    }]);

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 30.0, 10.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    assert!(list.items().iter().any(|i| matches!(i, DisplayItem::LinearGradient(_))));
}

#[test]
fn fragment_background_image_emits_image_item() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::Url(
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><rect width=\"1\" height=\"1\" fill=\"blue\"/></svg>"
                .into(),
        )),
        repeat: BackgroundRepeat::no_repeat(),
        ..Default::default()
    }]);
    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    let image = list.items().iter().find_map(|i| {
        if let DisplayItem::Image(img) = i {
            Some(img)
        } else {
            None
        }
    });
    let image = image.expect("background image should emit an image item");
    assert_eq!(image.dest_rect.width(), 1.0);
    assert_eq!(image.dest_rect.height(), 1.0);
}

#[test]
fn display_list_background_layers_paint_top_to_bottom() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    let top = BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![
                ColorStop {
                    color: Color::Rgba(Rgba::from_rgba8(0, 255, 0, 128)),
                    position: Some(0.0),
                },
                ColorStop {
                    color: Color::Rgba(Rgba::from_rgba8(0, 255, 0, 128)),
                    position: Some(1.0),
                },
            ],
        }),
        ..Default::default()
    };
    let bottom = BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![
                ColorStop {
                    color: Color::Rgba(Rgba::BLUE),
                    position: Some(0.0),
                },
                ColorStop {
                    color: Color::Rgba(Rgba::BLUE),
                    position: Some(1.0),
                },
            ],
        }),
        ..Default::default()
    };
    style.set_background_layers(vec![top, bottom]);

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));
    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);

    let gradients: Vec<_> = list
        .items()
        .iter()
        .filter_map(|i| match i {
            DisplayItem::LinearGradient(item) => Some(item),
            _ => None,
        })
        .collect();
    assert_eq!(gradients.len(), 2, "expected two background gradient items");
    // First emitted gradient should be the bottom layer (blue), second the top layer (green overlay).
    let bottom_color = gradients[0].stops[0].color;
    let top_color = gradients[1].stops[0].color;
    assert_eq!(bottom_color, Rgba::BLUE);
    assert_eq!(top_color, Rgba::from_rgba8(0, 255, 0, 128));
}

#[test]
fn display_list_background_layers_use_per_layer_clip() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    style.padding_left = fastrender::style::values::Length::px(4.0);
    style.padding_right = fastrender::style::values::Length::px(4.0);
    style.padding_top = fastrender::style::values::Length::px(4.0);
    style.padding_bottom = fastrender::style::values::Length::px(4.0);

    let top = BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![ColorStop {
                color: Color::Rgba(Rgba::GREEN),
                position: Some(0.0),
            }],
        }),
        clip: BackgroundBox::ContentBox,
        ..Default::default()
    };
    let bottom = BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![ColorStop {
                color: Color::Rgba(Rgba::BLUE),
                position: Some(0.0),
            }],
        }),
        clip: BackgroundBox::BorderBox,
        ..Default::default()
    };
    style.set_background_layers(vec![top, bottom]);

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![], Arc::new(style));
    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);

    let gradients: Vec<_> = list
        .items()
        .iter()
        .filter_map(|i| match i {
            DisplayItem::LinearGradient(item) => Some(item),
            _ => None,
        })
        .collect();
    assert_eq!(gradients.len(), 2, "expected two background gradient items");
    // Bottom layer should cover border box, top should be clipped to content box (20x20 minus 4px padding).
    assert_eq!(gradients[0].rect, Rect::from_xywh(0.0, 0.0, 20.0, 20.0));
    assert_eq!(gradients[1].rect, Rect::from_xywh(4.0, 4.0, 12.0, 12.0));
}

#[test]
fn background_repeat_space_centers_single_tile_in_display_list() {
    // 1x1 SVG so decoding succeeds without file I/O.
    let data_url = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><rect width=\"1\" height=\"1\" fill=\"#f00\"/></svg>".to_string();

    let mut style = ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::Url(data_url)),
        repeat: BackgroundRepeat {
            x: fastrender::style::types::BackgroundRepeatKeyword::Space,
            y: fastrender::style::types::BackgroundRepeatKeyword::Space,
        },
        size: fastrender::style::types::BackgroundSize::Explicit(
            fastrender::style::types::BackgroundSizeComponent::Length(Length::px(30.0)),
            fastrender::style::types::BackgroundSizeComponent::Length(Length::px(30.0)),
        ),
        ..Default::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
        Rect::from_xywh(0.0, 0.0, 40.0, 40.0),
        vec![],
        Arc::new(style),
    );

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    let image = list.items().iter().find_map(|item| {
        if let DisplayItem::Image(img) = item {
            Some(img)
        } else {
            None
        }
    });

    let image = image.expect("background image should emit an image item");
    assert_eq!(image.dest_rect.width(), 30.0);
    assert_eq!(image.dest_rect.height(), 30.0);
    assert!((image.dest_rect.x() - 5.0).abs() < 1e-3, "tile should be centered in 40px area");
    assert!((image.dest_rect.y() - 5.0).abs() < 1e-3, "tile should be centered vertically");
}

#[test]
fn background_attachment_fixed_anchors_to_viewport() {
    // Inline SVG (1x1) so decoding succeeds without filesystem IO.
    let data_url = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><rect width=\"1\" height=\"1\" fill=\"#0f0\"/></svg>".to_string();

    let mut style = ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::Url(data_url)),
        attachment: fastrender::style::types::BackgroundAttachment::Fixed,
        repeat: BackgroundRepeat::no_repeat(),
        size: fastrender::style::types::BackgroundSize::Explicit(
            fastrender::style::types::BackgroundSizeComponent::Length(Length::px(10.0)),
            fastrender::style::types::BackgroundSizeComponent::Length(Length::px(10.0)),
        ),
        ..Default::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
        Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
        vec![],
        Arc::new(style),
    );

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new()
        .with_viewport_size(100.0, 100.0)
        .build(&fragment);

    let image = list.items().iter().find_map(|item| match item {
        DisplayItem::Image(img) => Some(img),
        _ => None,
    });

    let image = image.expect("background image should emit an image item");
    // Attachment fixed should position relative to the viewport origin (0,0), not the fragment offset.
    assert!((image.dest_rect.x() - 0.0).abs() < 1e-3);
    assert!((image.dest_rect.y() - 0.0).abs() < 1e-3);
    assert_eq!(image.dest_rect.width(), 10.0);
    assert_eq!(image.dest_rect.height(), 10.0);
}
#[test]
fn fragment_box_shadow_emits_items() {
    let mut style = fastrender::ComputedStyle::default();
    style.box_shadow = vec![
        BoxShadow {
            offset_x: fastrender::style::values::Length::px(2.0),
            offset_y: fastrender::style::values::Length::px(3.0),
            blur_radius: fastrender::style::values::Length::px(4.0),
            spread_radius: fastrender::style::values::Length::px(1.0),
            color: Rgba::BLACK,
            inset: false,
        },
        BoxShadow {
            offset_x: fastrender::style::values::Length::px(-1.0),
            offset_y: fastrender::style::values::Length::px(-2.0),
            blur_radius: fastrender::style::values::Length::px(0.0),
            spread_radius: fastrender::style::values::Length::px(0.0),
            color: Rgba::RED,
            inset: true,
        },
    ];

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    let shadows: Vec<_> = list
        .items()
        .iter()
        .filter_map(|i| {
            if let DisplayItem::BoxShadow(s) = i {
                Some(s)
            } else {
                None
            }
        })
        .collect();
    assert_eq!(shadows.len(), 2, "expected inset and outset shadows");
    assert!(shadows.iter().any(|s| !s.inset && s.offset == Point::new(2.0, 3.0)));
    assert!(shadows.iter().any(|s| s.inset && s.color == Rgba::RED));
}

#[test]
fn fragment_opacity_wraps_display_items() {
    let mut style = fastrender::ComputedStyle::default();
    style.opacity = 0.5;
    style.color = Rgba::BLACK;
    let fragment = fastrender::FragmentNode {
        bounds: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
        content: fastrender::FragmentContent::Text {
            text: "hi".to_string(),
            baseline_offset: 8.0,
            shaped: None,
            box_id: None,
            is_marker: false,
        },
        baseline: None,
        children: vec![],
        style: Some(Arc::new(style)),
    };

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    let items = list.items();
    assert_eq!(items.len(), 3, "opacity should wrap content");
    assert!(matches!(items[0], DisplayItem::PushOpacity(_)));
    assert!(matches!(items[1], DisplayItem::Text(_)));
    assert!(matches!(items[2], DisplayItem::PopOpacity));
}

// ============================================================================
// FillRect Tests
// ============================================================================

#[test]
fn test_fill_rect_item() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(10.0, 20.0, 100.0, 50.0),
        color: Rgba::new(255, 0, 0, 1.0),
    }));

    assert_eq!(list.len(), 1);

    if let DisplayItem::FillRect(item) = &list.items()[0] {
        assert_eq!(item.rect.x(), 10.0);
        assert_eq!(item.rect.y(), 20.0);
        assert_eq!(item.rect.width(), 100.0);
        assert_eq!(item.rect.height(), 50.0);
        assert_eq!(item.color.r, 255);
    } else {
        panic!("Expected FillRect item");
    }
}

// ============================================================================
// StrokeRect Tests
// ============================================================================

#[test]
fn test_stroke_rect_item() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::StrokeRect(StrokeRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
        color: Rgba::BLUE,
        width: 2.0,
        blend_mode: BlendMode::Normal,
    }));

    assert_eq!(list.len(), 1);

    if let DisplayItem::StrokeRect(item) = &list.items()[0] {
        assert_eq!(item.width, 2.0);
        assert_eq!(item.color, Rgba::BLUE);
    } else {
        panic!("Expected StrokeRect item");
    }
}

// ============================================================================
// Rounded Rect Tests
// ============================================================================

#[test]
fn test_fill_rounded_rect() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::GREEN,
        radii: BorderRadii::uniform(10.0),
    }));

    if let DisplayItem::FillRoundedRect(item) = &list.items()[0] {
        assert!(item.radii.is_uniform());
        assert_eq!(item.radii.top_left, 10.0);
    } else {
        panic!("Expected FillRoundedRect item");
    }
}

#[test]
fn test_stroke_rounded_rect() {
    let mut list = DisplayList::new();

    let radii = BorderRadii::new(5.0, 10.0, 15.0, 20.0);

    list.push(DisplayItem::StrokeRoundedRect(StrokeRoundedRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::BLACK,
        width: 1.0,
        radii,
    }));

    if let DisplayItem::StrokeRoundedRect(item) = &list.items()[0] {
        assert!(!item.radii.is_uniform());
        assert_eq!(item.radii.top_left, 5.0);
        assert_eq!(item.radii.top_right, 10.0);
        assert_eq!(item.radii.bottom_right, 15.0);
        assert_eq!(item.radii.bottom_left, 20.0);
    } else {
        panic!("Expected StrokeRoundedRect item");
    }
}

// ============================================================================
// Text Item Tests
// ============================================================================

#[test]
fn test_text_item() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::Text(TextItem {
        origin: Point::new(100.0, 200.0),
        glyphs: vec![
            GlyphInstance {
                glyph_id: 72, // 'H'
                offset: Point::new(0.0, 0.0),
                advance: 12.0,
            },
            GlyphInstance {
                glyph_id: 101, // 'e'
                offset: Point::new(12.0, 0.0),
                advance: 10.0,
            },
        ],
        color: Rgba::BLACK,
        shadows: Vec::new(),
        font_size: 16.0,
        advance_width: 22.0,
        font_id: None,
        synthetic_bold: 0.0,
        synthetic_oblique: 0.0,
        emphasis: None,
        decorations: Vec::new(),
    }));

    if let DisplayItem::Text(item) = &list.items()[0] {
        assert_eq!(item.glyphs.len(), 2);
        assert_eq!(item.font_size, 16.0);
        assert_eq!(item.advance_width, 22.0);
    } else {
        panic!("Expected Text item");
    }
}

#[test]
fn sideways_writing_mode_emits_vertical_text_and_decorations() {
    let font = match FontContext::new().get_sans_serif() {
        Some(f) => Arc::new(f),
        None => return, // Skip if no fonts available
    };

    let glyph = GlyphPosition {
        glyph_id: 1,
        cluster: 0,
        x_offset: 5.0,
        y_offset: 0.0,
        x_advance: 0.0,
        y_advance: 0.0,
    };

    let run = ShapedRun {
        text: "A".to_string(),
        start: 0,
        end: 1,
        glyphs: vec![glyph],
        direction: Direction::LeftToRight,
        level: 0,
        advance: 0.0,
        font,
        font_size: 16.0,
        baseline_shift: 0.0,
        language: None,
        synthetic_bold: 0.0,
        synthetic_oblique: 0.0,
        rotation: RunRotation::None,
        scale: 1.0,
    };

    let mut style = ComputedStyle::default();
    style.writing_mode = WritingMode::SidewaysLr;
    style.text_decoration.lines = TextDecorationLine::UNDERLINE;

    let fragment = FragmentNode::new_text_shaped(
        Rect::from_xywh(0.0, 0.0, 20.0, 40.0),
        "A".to_string(),
        0.0,
        vec![run],
        Arc::new(style),
    );

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);

    let text_item = list
        .items()
        .iter()
        .find_map(|i| match i {
            DisplayItem::Text(t) => Some(t),
            _ => None,
        })
        .expect("expected text item");

    assert_eq!(text_item.glyphs.len(), 1);
    let glyph = &text_item.glyphs[0];
    assert_eq!(glyph.offset.x, 0.0);
    assert_eq!(glyph.offset.y, 5.0);

    let decoration = list
        .items()
        .iter()
        .find_map(|i| match i {
            DisplayItem::TextDecoration(d) => Some(d),
            _ => None,
        })
        .expect("expected text decoration item");

    assert!(decoration.inline_vertical);
    assert_eq!(decoration.line_start, 0.0);
    assert_eq!(decoration.line_width, 40.0);
}

// ============================================================================
// Image Item Tests
// ============================================================================

#[test]
fn test_image_item() {
    let mut list = DisplayList::new();

    let pixels = vec![255u8; 32 * 32 * 4]; // 32x32 RGBA image
    let image_data = Arc::new(ImageData::new_pixels(32, 32, pixels));

    list.push(DisplayItem::Image(ImageItem {
        dest_rect: Rect::from_xywh(10.0, 10.0, 64.0, 64.0),
        image: image_data.clone(),
        filter_quality: ImageFilterQuality::Linear,
        src_rect: None,
    }));

    if let DisplayItem::Image(item) = &list.items()[0] {
        assert_eq!(item.dest_rect.width(), 64.0);
        assert_eq!(item.image.width, 32);
        assert_eq!(item.image.height, 32);
        assert!(item.src_rect.is_none());
    } else {
        panic!("Expected Image item");
    }
}

#[test]
fn test_image_with_src_rect() {
    let pixels = vec![0u8; 100 * 100 * 4];
    let image_data = Arc::new(ImageData::new_pixels(100, 100, pixels));

    let item = ImageItem {
        dest_rect: Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
        image: image_data,
        filter_quality: ImageFilterQuality::Linear,
        src_rect: Some(Rect::from_xywh(0.0, 0.0, 50.0, 50.0)), // Top-left quadrant
    };

    assert!(item.src_rect.is_some());
}

// ============================================================================
// Box Shadow Tests
// ============================================================================

#[test]
fn test_box_shadow_item() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::BoxShadow(BoxShadowItem {
        rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
        radii: BorderRadii::uniform(5.0),
        offset: Point::new(5.0, 5.0),
        blur_radius: 10.0,
        spread_radius: 2.0,
        color: Rgba::new(0, 0, 0, 0.5),
        inset: false,
    }));

    if let DisplayItem::BoxShadow(item) = &list.items()[0] {
        assert_eq!(item.blur_radius, 10.0);
        assert_eq!(item.spread_radius, 2.0);
        assert!(!item.inset);

        // Check bounds include shadow expansion
        let display_item = &list.items()[0];
        let bounds = display_item.bounds().unwrap();
        // Bounds should be inflated by blur + spread
        assert!(bounds.width() > 100.0);
        assert!(bounds.height() > 100.0);
    } else {
        panic!("Expected BoxShadow item");
    }
}

#[test]
fn test_inset_box_shadow() {
    let item = BoxShadowItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        radii: BorderRadii::ZERO,
        offset: Point::new(0.0, 2.0),
        blur_radius: 4.0,
        spread_radius: 0.0,
        color: Rgba::BLACK,
        inset: true,
    };

    assert!(item.inset);
}

// ============================================================================
// Gradient Tests
// ============================================================================

#[test]
fn test_linear_gradient() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::LinearGradient(LinearGradientItem {
        rect: Rect::from_xywh(0.0, 0.0, 200.0, 100.0),
        start: Point::new(0.0, 0.0),
        end: Point::new(200.0, 0.0),
        spread: GradientSpread::Pad,
        stops: vec![
            GradientStop {
                position: 0.0,
                color: Rgba::RED,
            },
            GradientStop {
                position: 1.0,
                color: Rgba::BLUE,
            },
        ],
    }));

    if let DisplayItem::LinearGradient(item) = &list.items()[0] {
        assert_eq!(item.stops.len(), 2);
        assert_eq!(item.stops[0].position, 0.0);
        assert_eq!(item.stops[1].position, 1.0);
    } else {
        panic!("Expected LinearGradient item");
    }
}

#[test]
fn test_radial_gradient() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::RadialGradient(RadialGradientItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        center: Point::new(50.0, 50.0),
        radii: Point::new(50.0, 50.0),
        spread: GradientSpread::Pad,
        stops: vec![
            GradientStop {
                position: 0.0,
                color: Rgba::WHITE,
            },
            GradientStop {
                position: 0.5,
                color: Rgba::new(128, 128, 128, 1.0),
            },
            GradientStop {
                position: 1.0,
                color: Rgba::BLACK,
            },
        ],
    }));

    if let DisplayItem::RadialGradient(item) = &list.items()[0] {
        assert_eq!(item.stops.len(), 3);
        assert_eq!(item.center.x, 50.0);
        assert_eq!(item.radii, Point::new(50.0, 50.0));
    } else {
        panic!("Expected RadialGradient item");
    }
}

// ============================================================================
// Effect Stack Tests
// ============================================================================

#[test]
fn test_push_pop_clip() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushClip(ClipItem {
        shape: ClipShape::Rect {
            rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
            radii: None,
        },
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 200.0, 200.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::PopClip);

    assert_eq!(list.len(), 3);
    assert!(list.items()[0].is_stack_operation());
    assert!(!list.items()[1].is_stack_operation());
    assert!(list.items()[2].is_stack_operation());
}

#[test]
fn test_rounded_clip() {
    let item = ClipItem {
        shape: ClipShape::Rect {
            rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
            radii: Some(BorderRadii::uniform(10.0)),
        },
    };

    match &item.shape {
        ClipShape::Rect { radii, .. } => assert!(radii.is_some()),
        _ => panic!("expected rect clip"),
    }
}

#[test]
fn test_push_pop_opacity() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::RED,
    }));
    list.push(DisplayItem::PopOpacity);

    assert_eq!(list.len(), 3);

    if let DisplayItem::PushOpacity(item) = &list.items()[0] {
        assert_eq!(item.opacity, 0.5);
    } else {
        panic!("Expected PushOpacity");
    }
}

#[test]
fn test_push_pop_transform() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushTransform(TransformItem {
        transform: Transform2D::translate(100.0, 50.0),
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
        color: Rgba::GREEN,
    }));

    list.push(DisplayItem::PopTransform);

    assert_eq!(list.len(), 3);
}

// ============================================================================
// Transform2D Tests
// ============================================================================

#[test]
fn test_transform_composition() {
    // Test composing multiple transforms
    let translate = Transform2D::translate(10.0, 20.0);
    let scale = Transform2D::scale(2.0, 2.0);
    let rotate = Transform2D::rotate(std::f32::consts::FRAC_PI_4); // 45 degrees

    // Compose: scale -> translate
    let combined = translate.multiply(&scale);
    let p = Point::new(5.0, 5.0);
    let result = combined.transform_point(p);

    // Expected: scale (5,5) to (10,10), then translate to (20, 30)
    assert_eq!(result.x, 20.0);
    assert_eq!(result.y, 30.0);

    // Test rotation composition
    let rotate_twice = rotate.multiply(&rotate); // 90 degrees total
    let rotated = rotate_twice.transform_point(Point::new(1.0, 0.0));
    // After 90 degree rotation, (1,0) should become approximately (0,1)
    assert!((rotated.x - 0.0).abs() < 0.001);
    assert!((rotated.y - 1.0).abs() < 0.001);
}

#[test]
fn test_transform_skew() {
    let skew = Transform2D::skew(0.5, 0.0); // Skew in X direction
    let p = Point::new(0.0, 1.0);
    let result = skew.transform_point(p);

    // With X skew, y coordinate affects x position
    assert!((result.x - 0.5463).abs() < 0.001); // tan(0.5) ≈ 0.5463
    assert_eq!(result.y, 1.0);
}

#[test]
fn test_transform_inverse_roundtrip() {
    let t = Transform2D::translate(10.0, 20.0)
        .multiply(&Transform2D::scale(2.0, 0.5))
        .multiply(&Transform2D::rotate(0.3));

    let inv = t.inverse().expect("Transform should be invertible");
    let identity = t.multiply(&inv);

    // Should be close to identity
    assert!((identity.a - 1.0).abs() < 0.001);
    assert!((identity.d - 1.0).abs() < 0.001);
    assert!((identity.e).abs() < 0.001);
    assert!((identity.f).abs() < 0.001);
}

// ============================================================================
// Display List Bounds Tests
// ============================================================================

#[test]
fn test_display_list_bounds_computation() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(10.0, 10.0, 50.0, 50.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(100.0, 100.0, 50.0, 50.0),
        color: Rgba::BLUE,
    }));

    let bounds = list.bounds();

    // Bounds should encompass both rectangles
    assert_eq!(bounds.x(), 10.0);
    assert_eq!(bounds.y(), 10.0);
    assert_eq!(bounds.max_x(), 150.0);
    assert_eq!(bounds.max_y(), 150.0);
}

#[test]
fn test_empty_display_list_bounds() {
    let mut list = DisplayList::new();
    let bounds = list.bounds();

    assert_eq!(bounds, Rect::ZERO);
}

// ============================================================================
// Culling Tests
// ============================================================================

#[test]
fn test_culling_removes_outside_items() {
    let mut list = DisplayList::new();

    // Item inside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    // Item completely outside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(500.0, 500.0, 100.0, 100.0),
        color: Rgba::BLUE,
    }));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let culled = list.cull(viewport);

    assert_eq!(culled.len(), 1);
}

#[test]
fn test_culling_keeps_partial_intersections() {
    let mut list = DisplayList::new();

    // Item partially intersecting viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(150.0, 150.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let culled = list.cull(viewport);

    assert_eq!(culled.len(), 1);
}

#[test]
fn test_culling_preserves_stack_operations() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.5 }));

    // Item outside viewport
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(500.0, 500.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::PopOpacity);

    let viewport = Rect::from_xywh(0.0, 0.0, 200.0, 200.0);
    let culled = list.cull(viewport);

    // Stack operations should be preserved
    assert_eq!(culled.len(), 2);
    assert!(culled.items()[0].is_stack_operation());
    assert!(culled.items()[1].is_stack_operation());
}

// ============================================================================
// Optimization Tests
// ============================================================================

#[test]
fn test_optimize_removes_transparent_fills() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
        color: Rgba::TRANSPARENT,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(100.0, 100.0, 100.0, 100.0),
        color: Rgba::BLUE,
    }));

    list.optimize();

    // Transparent item should be removed
    assert_eq!(list.len(), 2);
}

#[test]
fn test_optimize_removes_transparent_strokes() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::StrokeRect(StrokeRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::BLACK,
        width: 1.0,
        blend_mode: BlendMode::Normal,
    }));

    list.push(DisplayItem::StrokeRect(StrokeRectItem {
        rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
        color: Rgba::TRANSPARENT,
        width: 2.0,
        blend_mode: BlendMode::Normal,
    }));

    list.optimize();

    assert_eq!(list.len(), 1);
}

#[test]
fn test_optimize_keeps_non_transparent_shadows() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::BoxShadow(BoxShadowItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        radii: BorderRadii::ZERO,
        offset: Point::new(5.0, 5.0),
        blur_radius: 10.0,
        spread_radius: 0.0,
        color: Rgba::new(0, 0, 0, 0.5), // Semi-transparent
        inset: false,
    }));

    list.optimize();

    assert_eq!(list.len(), 1);
}

// ============================================================================
// BorderRadii Tests
// ============================================================================

#[test]
fn test_border_radii_defaults() {
    let radii = BorderRadii::default();
    assert_eq!(radii, BorderRadii::ZERO);
    assert!(!radii.has_radius());
}

#[test]
fn test_border_radii_max() {
    let radii = BorderRadii::new(5.0, 15.0, 10.0, 20.0);
    assert_eq!(radii.max_radius(), 20.0);
}

// ============================================================================
// BlendMode Tests
// ============================================================================

#[test]
fn test_blend_modes() {
    use fastrender::BlendModeItem;

    // Test various blend modes are available
    let modes = vec![
        BlendMode::Normal,
        BlendMode::Multiply,
        BlendMode::Screen,
        BlendMode::Overlay,
        BlendMode::Darken,
        BlendMode::Lighten,
        BlendMode::ColorDodge,
        BlendMode::ColorBurn,
        BlendMode::HardLight,
        BlendMode::SoftLight,
        BlendMode::Difference,
        BlendMode::Exclusion,
        BlendMode::Hue,
        BlendMode::Saturation,
        BlendMode::Color,
        BlendMode::Luminosity,
    ];

    let mut list = DisplayList::new();

    for mode in &modes {
        list.push(DisplayItem::PushBlendMode(BlendModeItem { mode: *mode }));
        list.push(DisplayItem::PopBlendMode);
    }

    assert_eq!(list.len(), modes.len() * 2);
}

// ============================================================================
// Complex Scenario Tests
// ============================================================================

#[test]
fn test_complex_display_list() {
    let mut list = DisplayList::with_capacity(20);

    // Background
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 800.0, 600.0),
        color: Rgba::WHITE,
    }));

    // Box shadow
    list.push(DisplayItem::BoxShadow(BoxShadowItem {
        rect: Rect::from_xywh(100.0, 100.0, 200.0, 200.0),
        radii: BorderRadii::uniform(8.0),
        offset: Point::new(0.0, 4.0),
        blur_radius: 8.0,
        spread_radius: 0.0,
        color: Rgba::new(0, 0, 0, 0.25),
        inset: false,
    }));

    // Container with rounded corners
    list.push(DisplayItem::FillRoundedRect(FillRoundedRectItem {
        rect: Rect::from_xywh(100.0, 100.0, 200.0, 200.0),
        color: Rgba::new(245, 245, 245, 1.0),
        radii: BorderRadii::uniform(8.0),
    }));

    // Push opacity for content
    list.push(DisplayItem::PushOpacity(OpacityItem { opacity: 0.8 }));

    // Text
    list.push(DisplayItem::Text(TextItem {
        origin: Point::new(120.0, 150.0),
        glyphs: vec![],
        color: Rgba::BLACK,
        shadows: Vec::new(),
        font_size: 16.0,
        advance_width: 100.0,
        font_id: None,
        synthetic_bold: 0.0,
        synthetic_oblique: 0.0,
        emphasis: None,
        decorations: Vec::new(),
    }));

    // Border
    list.push(DisplayItem::StrokeRoundedRect(StrokeRoundedRectItem {
        rect: Rect::from_xywh(100.0, 100.0, 200.0, 200.0),
        color: Rgba::new(200, 200, 200, 1.0),
        width: 1.0,
        radii: BorderRadii::uniform(8.0),
    }));

    list.push(DisplayItem::PopOpacity);

    assert_eq!(list.len(), 7);

    // Test bounds
    let bounds = list.clone().bounds();
    assert!(bounds.width() > 200.0); // Includes shadow
    assert!(bounds.height() > 200.0);
}

#[test]
fn test_nested_transforms() {
    let mut list = DisplayList::new();

    // Outer transform
    list.push(DisplayItem::PushTransform(TransformItem {
        transform: Transform2D::translate(100.0, 100.0),
    }));

    // Inner transform
    list.push(DisplayItem::PushTransform(TransformItem {
        transform: Transform2D::scale(2.0, 2.0),
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 50.0, 50.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::PopTransform);
    list.push(DisplayItem::PopTransform);

    assert_eq!(list.len(), 5);
}

#[test]
fn test_stacking_context() {
    use fastrender::StackingContextItem;

    let mut list = DisplayList::new();

    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 10,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        mix_blend_mode: BlendMode::Normal,
        is_isolated: false,
        transform: None,
        filters: Vec::new(),
        backdrop_filters: Vec::new(),
        radii: BorderRadii::ZERO,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::PopStackingContext);

    assert_eq!(list.len(), 3);
    assert!(list.items()[0].is_stack_operation());
    assert!(list.items()[2].is_stack_operation());
}

#[test]
fn paint_containment_clips_stacking_context() {
    let mut style = fastrender::ComputedStyle::default();
    style.containment = Containment::with_flags(false, false, false, false, true);
    style.border_top_width = Length::px(2.0);
    style.border_right_width = Length::px(2.0);
    style.border_bottom_width = Length::px(2.0);
    style.border_left_width = Length::px(2.0);
    style.border_top_left_radius = Length::px(5.0);
    style.border_top_right_radius = Length::px(5.0);
    style.border_bottom_right_radius = Length::px(5.0);
    style.border_bottom_left_radius = Length::px(5.0);

    let fragment =
        fastrender::FragmentNode::new_block_styled(Rect::from_xywh(10.0, 20.0, 50.0, 30.0), vec![], Arc::new(style));

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let items = list.items();

    let clip_start = items
        .iter()
        .position(|item| matches!(item, DisplayItem::PushClip(_)))
        .expect("paint containment should push a clip");
    let clip_end = items
        .iter()
        .rposition(|item| matches!(item, DisplayItem::PopClip))
        .expect("paint containment should pop a clip");

    assert!(clip_end > clip_start, "clip should wrap the stacking context");
    let stacking_idx = items
        .iter()
        .position(|item| matches!(item, DisplayItem::PushStackingContext(_)))
        .expect("stacking context should be present");
    assert!(stacking_idx > clip_start && stacking_idx < clip_end);

    match &items[clip_start] {
        DisplayItem::PushClip(ClipItem {
            shape: ClipShape::Rect { rect, radii },
        }) => {
            assert_eq!(*rect, Rect::from_xywh(12.0, 22.0, 46.0, 26.0));
            assert_eq!(*radii, Some(BorderRadii::uniform(3.0)));
        }
        other => panic!("expected PushClip, got {:?}", other),
    }
}

#[test]
fn video_replaced_element_uses_poster_image_in_display_list() {
    // Create a tiny PNG poster on disk so the display list builder can decode it.
    let mut poster_path = std::env::temp_dir();
    poster_path.push(format!(
        "fastrender_dl_video_poster_{}.png",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    let mut img = image::RgbaImage::new(1, 1);
    img.put_pixel(0, 0, image::Rgba([0, 0, 0, 0]));
    img.save(&poster_path).expect("poster image saved");

    let replaced_type = ReplacedType::Video {
        src: "unused".to_string(),
        poster: Some(poster_path.to_string_lossy().to_string()),
    };
    let style = Arc::new(ComputedStyle::default());
    let fragment = FragmentNode::new_with_style(
        Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
        FragmentContent::Replaced {
            replaced_type: replaced_type.clone(),
            box_id: None,
        },
        vec![],
        style,
    );

    let list = fastrender::paint::display_list_builder::DisplayListBuilder::new().build(&fragment);
    let image = list.items().iter().find_map(|item| {
        if let DisplayItem::Image(img) = item {
            Some(img)
        } else {
            None
        }
    });
    let image = image.expect("video replaced element should emit an image from poster");
    assert_eq!(image.dest_rect, Rect::from_xywh(0.0, 0.0, 10.0, 10.0));
    let _ = std::fs::remove_file(&poster_path);
}

// ============================================================================
// Extend Tests
// ============================================================================

#[test]
fn test_display_list_extend() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    let more_items = vec![
        DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
            color: Rgba::GREEN,
        }),
        DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(100.0, 100.0, 100.0, 100.0),
            color: Rgba::BLUE,
        }),
    ];

    list.extend(more_items);

    assert_eq!(list.len(), 3);
}

#[test]
fn test_display_list_clear() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(50.0, 50.0, 100.0, 100.0),
        color: Rgba::BLUE,
    }));

    assert_eq!(list.len(), 2);

    list.clear();

    assert!(list.is_empty());
    assert_eq!(list.len(), 0);
}

// ============================================================================
// Iterator Tests
// ============================================================================

#[test]
fn test_display_list_iter() {
    let mut list = DisplayList::new();

    for i in 0..5 {
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(i as f32 * 10.0, 0.0, 10.0, 10.0),
            color: Rgba::RED,
        }));
    }

    let count = list.iter().count();
    assert_eq!(count, 5);
}

#[test]
fn test_display_list_display() {
    let mut list = DisplayList::new();

    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 100.0, 100.0),
        color: Rgba::RED,
    }));

    let display_str = format!("{}", list);
    assert!(display_str.contains("1 items"));
}
