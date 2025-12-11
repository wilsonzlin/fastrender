use fastrender::geometry::Rect;
use fastrender::paint::display_list::{DisplayItem, DisplayList, FillRectItem, ResolvedFilter, StackingContextItem};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::text::font_loader::FontContext;
use fastrender::Rgba;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let px = pixmap.pixel(x, y).unwrap();
    (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn filters_apply_to_stacking_context_layer() {
    let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
        is_isolated: true,
        transform: None,
        filters: vec![ResolvedFilter::Invert(1.0)],
        backdrop_filters: Vec::new(),
        radii: fastrender::paint::display_list::BorderRadii::ZERO,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::BLUE,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Inverting blue yields yellow.
    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 0, 255));
}

#[test]
fn backdrop_filters_modify_backdrop_region() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        color: Rgba::RED,
    }));
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
        mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
        is_isolated: false,
        transform: None,
        filters: Vec::new(),
        backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
        radii: fastrender::paint::display_list::BorderRadii::ZERO,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Inside backdrop-filtered region red should invert to cyan.
    assert_eq!(pixel(&pixmap, 1, 1), (0, 255, 255, 255));
    // Outside region remains red.
    assert_eq!(pixel(&pixmap, 0, 0), (255, 0, 0, 255));
}

#[test]
fn drop_shadow_filter_renders_shadow() {
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
        mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
        is_isolated: true,
        transform: None,
        filters: vec![ResolvedFilter::DropShadow {
            offset_x: 1.0,
            offset_y: 0.0,
            blur_radius: 0.0,
            spread: 0.0,
            color: Rgba::BLACK,
        }],
        backdrop_filters: Vec::new(),
        radii: fastrender::paint::display_list::BorderRadii::ZERO,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::BLUE,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Shadow offset right by 1px should leave black pixel at (2,0).
    assert_eq!(pixel(&pixmap, 2, 0), (0, 0, 0, 255));
    // Original content stays blue.
    assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 255, 255));
}

#[test]
fn color_blend_mode_uses_destination_luminance() {
    use fastrender::paint::display_list::{BlendMode, BlendModeItem};

    let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    // Destination: mid-gray.
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(128, 128, 128, 255),
    }));
    // Apply color blend with vivid red source: hue/saturation from source, luminance from destination.
    list.push(DisplayItem::PushBlendMode(BlendModeItem {
        mode: BlendMode::Color,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::RED,
    }));
    list.push(DisplayItem::PopBlendMode);

    let pixmap = renderer.render(&list).unwrap();
    let (r, g, b, a) = pixel(&pixmap, 0, 0);
    assert_eq!(a, 255);
    // Color blend should keep destination luminance (~128) while adopting the source hue/saturation.
    let luminance = 0.3 * r as f32 + 0.59 * g as f32 + 0.11 * b as f32;
    assert!(
        (luminance - 128.0).abs() < 1.0,
        "expected destination luminance preserved (~128), got {luminance}"
    );
    assert!(
        r > 200 && g < 100 && b < 100 && (g as i16 - b as i16).abs() <= 1,
        "expected hue/saturation from red source, got ({r},{g},{b})"
    );
}
