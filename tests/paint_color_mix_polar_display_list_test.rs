use fastrender::geometry::Rect;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Color;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::Rgba;
use std::sync::Arc;

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let idx = (y * pixmap.width() + x) as usize * 4;
    (pixmap.data()[idx], pixmap.data()[idx + 1], pixmap.data()[idx + 2], pixmap.data()[idx + 3])
}

fn render(color: &str) -> (u8, u8, u8, u8) {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Color::parse(color).unwrap().to_rgba(Rgba::BLACK);

    let fragment = FragmentNode::new_block_styled(
        Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
        vec![],
        Arc::new(style),
    );

    let list = DisplayListBuilder::new().build(&fragment);
    let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(&list)
        .unwrap();
    pixel(&pixmap, 0, 0)
}

#[test]
fn color_mix_oklch_renders_expected() {
    let expected = Color::parse("color-mix(in oklch, red 50%, blue 50%)")
        .unwrap()
        .to_rgba(Rgba::BLACK);
    assert_eq!(render("color-mix(in oklch, red 50%, blue 50%)"), (expected.r, expected.g, expected.b, expected.alpha_u8()));
}
