use fastrender::geometry::Rect;
use fastrender::paint::display_list::{DecorationPaint, DecorationStroke, DisplayItem, TextDecorationItem};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::types::TextDecorationStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::Rgba;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let idx = (y * pixmap.width() + x) as usize * 4;
    (pixmap.data()[idx], pixmap.data()[idx + 1], pixmap.data()[idx + 2], pixmap.data()[idx + 3])
}

#[test]
fn display_list_skip_ink_none_renders_full_line() {
    let mut list = fastrender::paint::display_list::DisplayList::new();
    let decoration = DecorationPaint {
        style: TextDecorationStyle::Solid,
        color: Rgba::BLACK,
        underline: Some(DecorationStroke {
            center: 5.0,
            thickness: 2.0,
            segments: None,
        }),
        overline: None,
        line_through: None,
    };

    list.push(DisplayItem::TextDecoration(TextDecorationItem {
        bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
        line_start: 0.0,
        line_width: 10.0,
        decorations: vec![decoration],
        inline_vertical: false,
    }));

    let pixmap = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(&list)
        .unwrap();

    // Underline painted through the center; check a mid-line pixel is black.
    assert_eq!(pixel(&pixmap, 5, 5), (0, 0, 0, 255));
}

#[test]
fn display_list_skip_ink_segments_only_paint_provided_spans() {
    // Segments represent the painted spans; outside should remain unpainted.
    let mut list = fastrender::paint::display_list::DisplayList::new();
    let decoration = DecorationPaint {
        style: TextDecorationStyle::Solid,
        color: Rgba::BLACK,
        underline: Some(DecorationStroke {
            center: 5.0,
            thickness: 2.0,
            segments: Some(vec![(3.0, 7.0)]),
        }),
        overline: None,
        line_through: None,
    };

    list.push(DisplayItem::TextDecoration(TextDecorationItem {
        bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
        line_start: 0.0,
        line_width: 10.0,
        decorations: vec![decoration],
        inline_vertical: false,
    }));

    let pixmap = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(&list)
        .unwrap();

    // Pixel inside the painted span is black; outside stays white.
    assert_eq!(pixel(&pixmap, 4, 5), (0, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 1, 5), (255, 255, 255, 255));
    assert_eq!(pixel(&pixmap, 8, 5), (255, 255, 255, 255));
}
