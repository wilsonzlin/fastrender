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

