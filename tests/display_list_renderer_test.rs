use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use fastrender::css::types::ColorStop;
use fastrender::geometry::Rect;
use fastrender::paint::display_list::{
    DecorationPaint, DecorationStroke, DisplayItem, DisplayList, FillRectItem, ResolvedFilter, StackingContextItem,
    TextDecorationItem,
};
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Color;
use fastrender::style::types::{
    BackgroundImage, BackgroundLayer, BackgroundPosition, BackgroundPositionComponent, BasicShape, BorderImage,
    BorderImageSlice, BorderImageSliceValue, BorderImageSource, BorderStyle, ClipPath, FillRule, ShapeRadius,
    TextDecorationStyle,
};
use fastrender::style::values::Length;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::Rgba;
use image::{codecs::png::PngEncoder, ExtendedColorType, ImageEncoder, RgbaImage};
use std::sync::Arc;

fn rgb_to_hsl(r: u8, g: u8, b: u8) -> (f32, f32, f32) {
    let r = r as f32 / 255.0;
    let g = g as f32 / 255.0;
    let b = b as f32 / 255.0;
    let max = r.max(g).max(b);
    let min = r.min(g).min(b);
    let l = (max + min) / 2.0;

    if (max - min).abs() < f32::EPSILON {
        return (0.0, 0.0, l);
    }

    let d = max - min;
    let s = if l > 0.5 {
        d / (2.0 - max - min)
    } else {
        d / (max + min)
    };
    let h = if (max - r).abs() < f32::EPSILON {
        (g - b) / d + if g < b { 6.0 } else { 0.0 }
    } else if (max - g).abs() < f32::EPSILON {
        (b - r) / d + 2.0
    } else {
        (r - g) / d + 4.0
    } / 6.0;

    (h, s, l)
}

fn hue_to_rgb(p: f32, q: f32, t: f32) -> f32 {
    let mut t = t;
    if t < 0.0 {
        t += 1.0;
    }
    if t > 1.0 {
        t -= 1.0;
    }
    if t < 1.0 / 6.0 {
        return p + (q - p) * 6.0 * t;
    }
    if t < 1.0 / 2.0 {
        return q;
    }
    if t < 2.0 / 3.0 {
        return p + (q - p) * (2.0 / 3.0 - t) * 6.0;
    }
    p
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (u8, u8, u8) {
    if s <= 0.0 {
        let v = (l * 255.0).round().clamp(0.0, 255.0) as u8;
        return (v, v, v);
    }

    let q = if l < 0.5 { l * (1.0 + s) } else { l + s - l * s };
    let p = 2.0 * l - q;
    let r = hue_to_rgb(p, q, h + 1.0 / 3.0);
    let g = hue_to_rgb(p, q, h);
    let b = hue_to_rgb(p, q, h - 1.0 / 3.0);
    (
        (r * 255.0).round().clamp(0.0, 255.0) as u8,
        (g * 255.0).round().clamp(0.0, 255.0) as u8,
        (b * 255.0).round().clamp(0.0, 255.0) as u8,
    )
}

fn blend_hue(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
    let (sh, _, _) = rgb_to_hsl(src.0, src.1, src.2);
    let (_, ds, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
    hsl_to_rgb(sh, ds, dl)
}

fn blend_saturation(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
    let (_, ss, _) = rgb_to_hsl(src.0, src.1, src.2);
    let (dh, _, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
    hsl_to_rgb(dh, ss, dl)
}

fn blend_color(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
    let (sh, ss, _) = rgb_to_hsl(src.0, src.1, src.2);
    let (_, _, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
    hsl_to_rgb(sh, ss, dl)
}

fn blend_luminosity(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
    let (_, _, sl) = rgb_to_hsl(src.0, src.1, src.2);
    let (dh, ds, _) = rgb_to_hsl(dst.0, dst.1, dst.2);
    hsl_to_rgb(dh, ds, sl)
}

fn hue_delta(a: f32, b: f32) -> f32 {
    let d = (a - b).abs();
    d.min(1.0 - d)
}

fn assert_hsl_components(
    actual: (u8, u8, u8),
    expected_hsl: (f32, f32, f32),
    tol_h: f32,
    tol_s: f32,
    tol_l: f32,
    context: &str,
) {
    let (h, s, l) = rgb_to_hsl(actual.0, actual.1, actual.2);
    assert!(
        hue_delta(h, expected_hsl.0) <= tol_h
            && (s - expected_hsl.1).abs() <= tol_s
            && (l - expected_hsl.2).abs() <= tol_l,
        "{context}: expected hsl {:?}, got hsl ({h:.3},{s:.3},{l:.3})",
        expected_hsl
    );
}

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let px = pixmap.pixel(x, y).unwrap();
    (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn builder_clip_path_masks_rendered_output() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.clip_path = ClipPath::BasicShape(
        BasicShape::Circle {
            radius: ShapeRadius::Length(Length::px(3.0)),
            position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: 0.5,
                    offset: Length::px(0.0),
                },
                y: BackgroundPositionComponent {
                    alignment: 0.5,
                    offset: Length::px(0.0),
                },
            },
        },
        None,
    );

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    // Center pixel should be clipped in, corner should remain the clear background.
    assert_eq!(pixel(&pixmap, 5, 5), (255, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
}

#[test]
fn builder_clip_path_polygon_masks_rendered_output() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.clip_path = ClipPath::BasicShape(
        BasicShape::Polygon {
            fill: FillRule::NonZero,
            points: vec![
                (Length::px(0.0), Length::px(0.0)),
                (Length::px(0.0), Length::px(10.0)),
                (Length::px(10.0), Length::px(0.0)),
            ],
        },
        None,
    );

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    // Pixel inside triangle should be filled; bottom-right should stay white.
    assert_eq!(pixel(&pixmap, 2, 2), (255, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 9, 9), (255, 255, 255, 255));
}

#[test]
fn color_mix_srgb_renders_expected_color() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Color::parse("color-mix(in srgb, red 25%, blue 75%)")
        .unwrap()
        .to_rgba(Rgba::BLACK);

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build(&fragment);
    let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(&list)
        .unwrap();

    let expected = Color::parse("color-mix(in srgb, red 25%, blue 75%)")
        .unwrap()
        .to_rgba(Rgba::BLACK);
    assert_eq!(
        pixel(&pixmap, 0, 0),
        (expected.r, expected.g, expected.b, expected.alpha_u8())
    );
}

#[test]
fn color_mix_srgb_linear_matches_resolved_color() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Color::parse("color-mix(in srgb-linear, red 50%, blue 50%)")
        .unwrap()
        .to_rgba(Rgba::BLACK);

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build(&fragment);
    let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
        .unwrap()
        .render(&list)
        .unwrap();

    let expected = Color::parse("color-mix(in srgb-linear, red 50%, blue 50%)")
        .unwrap()
        .to_rgba(Rgba::BLACK);
    assert_eq!(
        pixel(&pixmap, 0, 0),
        (expected.r, expected.g, expected.b, expected.alpha_u8())
    );
}

#[test]
fn display_list_renderer_paints_text_decoration_color() {
    let mut list = DisplayList::new();
    let decoration = DecorationPaint {
        style: TextDecorationStyle::Solid,
        color: Rgba::RED,
        underline: Some(DecorationStroke {
            center: 5.0,
            thickness: 2.0,
            segments: None,
        }),
        overline: None,
        line_through: None,
    };

    list.push(DisplayItem::TextDecoration(TextDecorationItem {
        bounds: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
        line_start: 0.0,
        line_width: 20.0,
        inline_vertical: false,
        decorations: vec![decoration],
    }));

    let renderer = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    assert_eq!(pixel(&pixmap, 10, 5), (255, 0, 0, 255));
    assert_eq!(pixel(&pixmap, 10, 0), (255, 255, 255, 255));
}

#[test]
fn text_decoration_segments_offset_by_line_start() {
    // Ensure decoration segments are translated by line_start when rendering (skip-ink segments).
    let mut list = DisplayList::new();
    list.push(DisplayItem::TextDecoration(TextDecorationItem {
        bounds: Rect::from_xywh(0.0, 0.0, 30.0, 10.0),
        line_start: 10.0,
        line_width: 10.0,
        inline_vertical: false,
        decorations: vec![DecorationPaint {
            style: TextDecorationStyle::Solid,
            color: Rgba::BLACK,
            underline: Some(DecorationStroke {
                center: 5.0,
                thickness: 2.0,
                segments: Some(vec![(2.0, 4.0)]),
            }),
            overline: None,
            line_through: None,
        }],
    }));

    let renderer = DisplayListRenderer::new(30, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    // Painted pixels should appear around x ~12–13 (line_start + segment start/end).
    let mut min_x = u32::MAX;
    let mut max_x = 0;
    for (idx, chunk) in pixmap.data().chunks(4).enumerate() {
        if chunk[0] == 255 && chunk[1] == 255 && chunk[2] == 255 {
            continue;
        }
        let x = (idx as u32) % pixmap.width();
        min_x = min_x.min(x);
        max_x = max_x.max(x);
    }

    assert!(min_x >= 10 && min_x <= 13, "min_x out of expected range: {}", min_x);
    assert!(max_x >= 12 && max_x <= 14, "max_x out of expected range: {}", max_x);
}

#[test]
fn text_decoration_currentcolor_resolves_in_display_list() {
    // Ensure currentColor on text decorations resolves against the text color when emitting display list items.
    let mut list = DisplayList::new();
    let mut style = fastrender::ComputedStyle::default();
    style.color = Rgba::rgb(10, 20, 30);
    style.text_decoration.lines = fastrender::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = None; // currentColor
    style.text_decoration.thickness = fastrender::style::types::TextDecorationThickness::Length(Length::px(2.0));

    let mut fragment = FragmentNode::new_text(Rect::from_xywh(0.0, 0.0, 20.0, 10.0), "hi".to_string(), 0.0);
    fragment.style = Some(Arc::new(style));

    let list = DisplayListBuilder::new().build(&fragment);
    let deco = list
        .items()
        .iter()
        .find_map(|i| match i {
            DisplayItem::TextDecoration(d) => Some(d),
            _ => None,
        })
        .expect("decoration item");

    let underline = deco
        .decorations
        .first()
        .and_then(|d| d.underline.as_ref())
        .expect("underline present");

    let mut renderer = DisplayListRenderer::new(30, 20, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    // Verify some pixels are non-white (decoration rendered) and dominated by the text color channels.
    let mut max_r = 0u8;
    let mut max_g = 0u8;
    let mut max_b = 0u8;
    for chunk in pixmap.data().chunks(4) {
        if chunk[0] == 255 && chunk[1] == 255 && chunk[2] == 255 {
            continue;
        }
        max_r = max_r.max(chunk[0]);
        max_g = max_g.max(chunk[1]);
        max_b = max_b.max(chunk[2]);
    }
    assert!(
        max_r >= 10 && max_g >= 20 && max_b >= 30,
        "decoration should resolve currentColor"
    );
}

#[test]
fn display_list_border_image_nine_slice() {
    // Construct a 3x3 image with distinct corners/edges to verify nine-slice placement.
    let mut img = RgbaImage::new(3, 3);
    img.put_pixel(0, 0, image::Rgba([255, 0, 0, 255])); // TL red
    img.put_pixel(2, 0, image::Rgba([0, 0, 255, 255])); // TR blue
    img.put_pixel(0, 2, image::Rgba([0, 255, 0, 255])); // BL green
    img.put_pixel(2, 2, image::Rgba([255, 255, 0, 255])); // BR yellow
    let edge = image::Rgba([0, 255, 255, 255]);
    img.put_pixel(1, 0, edge);
    img.put_pixel(1, 2, edge);
    img.put_pixel(0, 1, edge);
    img.put_pixel(2, 1, edge);
    img.put_pixel(1, 1, image::Rgba([255, 255, 255, 255]));

    let mut buf = Vec::new();
    PngEncoder::new(&mut buf)
        .write_image(img.as_raw(), 3, 3, ExtendedColorType::Rgba8)
        .unwrap();
    let data_url = format!("data:image/png;base64,{}", STANDARD.encode(&buf));

    let mut style = fastrender::ComputedStyle::default();
    style.border_top_width = Length::px(4.0);
    style.border_right_width = Length::px(4.0);
    style.border_bottom_width = Length::px(4.0);
    style.border_left_width = Length::px(4.0);
    style.border_top_style = BorderStyle::Solid;
    style.border_right_style = BorderStyle::Solid;
    style.border_bottom_style = BorderStyle::Solid;
    style.border_left_style = BorderStyle::Solid;
    style.border_image = BorderImage {
        source: BorderImageSource::Image(BackgroundImage::Url(data_url)),
        slice: BorderImageSlice {
            top: BorderImageSliceValue::Number(1.0),
            right: BorderImageSliceValue::Number(1.0),
            bottom: BorderImageSliceValue::Number(1.0),
            left: BorderImageSliceValue::Number(1.0),
            fill: false,
        },
        ..BorderImage::default()
    };

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 16.0, 16.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let renderer = DisplayListRenderer::new(16, 16, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    let tl = pixel(&pixmap, 0, 0);
    let tr = pixel(&pixmap, 15, 0);
    let bl = pixel(&pixmap, 0, 15);
    let br = pixel(&pixmap, 15, 15);
    assert_eq!(tl, (255, 0, 0, 255));
    assert_eq!(tr, (0, 0, 255, 255));
    assert_eq!(bl, (0, 255, 0, 255));
    assert_eq!(br, (255, 255, 0, 255));

    let edge_top = pixel(&pixmap, 8, 1);
    let edge_left = pixel(&pixmap, 1, 8);
    assert_eq!(edge_top, (0, 255, 255, 255));
    assert_eq!(edge_left, (0, 255, 255, 255));
}

#[test]
fn background_attachment_local_clips_to_padding_box_in_display_list() {
    let mut style = fastrender::ComputedStyle::default();
    style.background_color = Rgba::TRANSPARENT;
    style.set_background_layers(vec![fastrender::style::types::BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![
                ColorStop {
                    color: fastrender::Color::Rgba(Rgba::RED),
                    position: Some(0.0),
                },
                ColorStop {
                    color: fastrender::Color::Rgba(Rgba::BLUE),
                    position: Some(1.0),
                },
            ],
        }),
        position: BackgroundPosition::Position {
            x: BackgroundPositionComponent {
                alignment: 0.0,
                offset: Length::px(0.0),
            },
            y: BackgroundPositionComponent {
                alignment: 0.0,
                offset: Length::px(0.0),
            },
        },
        size: fastrender::style::types::BackgroundSize::Keyword(
            fastrender::style::types::BackgroundSizeKeyword::Contain,
        ),
        repeat: fastrender::style::types::BackgroundRepeat {
            x: fastrender::style::types::BackgroundRepeatKeyword::NoRepeat,
            y: fastrender::style::types::BackgroundRepeatKeyword::NoRepeat,
        },
        origin: fastrender::style::types::BackgroundBox::PaddingBox,
        clip: fastrender::style::types::BackgroundBox::PaddingBox,
        attachment: fastrender::style::types::BackgroundAttachment::Local,
        ..Default::default()
    }]);
    style.padding_top = Length::px(2.0);
    style.padding_right = Length::px(2.0);
    style.padding_bottom = Length::px(2.0);
    style.padding_left = Length::px(2.0);
    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    // Background should anchor to the padding box; interior should paint.
    assert_ne!(pixel(&pixmap, 3, 3), (255, 255, 255, 255));
}

#[test]
fn display_list_border_image_generated_uniform_color() {
    let mut style = fastrender::ComputedStyle::default();
    style.border_top_width = Length::px(4.0);
    style.border_right_width = Length::px(4.0);
    style.border_bottom_width = Length::px(4.0);
    style.border_left_width = Length::px(4.0);
    style.border_top_style = BorderStyle::Solid;
    style.border_right_style = BorderStyle::Solid;
    style.border_bottom_style = BorderStyle::Solid;
    style.border_left_style = BorderStyle::Solid;
    style.border_image = BorderImage {
        source: BorderImageSource::Image(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![
                ColorStop {
                    color: fastrender::Color::Rgba(Rgba::RED),
                    position: Some(0.0),
                },
                ColorStop {
                    color: fastrender::Color::Rgba(Rgba::RED),
                    position: Some(1.0),
                },
            ],
        }),
        slice: BorderImageSlice {
            top: BorderImageSliceValue::Number(1.0),
            right: BorderImageSliceValue::Number(1.0),
            bottom: BorderImageSliceValue::Number(1.0),
            left: BorderImageSliceValue::Number(1.0),
            fill: false,
        },
        ..BorderImage::default()
    };

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 16.0, 16.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
    let renderer = DisplayListRenderer::new(16, 16, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    // Uniform gradient should behave like a solid image for border painting.
    let tl = pixel(&pixmap, 0, 0);
    let tr = pixel(&pixmap, 15, 0);
    let bl = pixel(&pixmap, 0, 15);
    let br = pixel(&pixmap, 15, 15);
    assert_eq!(tl, (255, 0, 0, 255));
    assert_eq!(tr, (255, 0, 0, 255));
    assert_eq!(bl, (255, 0, 0, 255));
    assert_eq!(br, (255, 0, 0, 255));
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
fn blur_filters_arent_clipped_by_border_radii() {
    use fastrender::paint::display_list::BorderRadii;

    let renderer = DisplayListRenderer::new(4, 4, Rgba::TRANSPARENT, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
        mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
        is_isolated: true,
        transform: None,
        filters: vec![ResolvedFilter::Blur(1.0)],
        backdrop_filters: Vec::new(),
        radii: BorderRadii::uniform(0.5),
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
        color: Rgba::RED,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Blur should spill outside the original bounds; left neighbor gains alpha.
    let (_, _, _, alpha) = pixel(&pixmap, 0, 1);
    assert!(
        alpha > 0,
        "expected blur outside rounded rect to remain visible; alpha at (0,1) was {alpha}"
    );
    // Farther away should be at most a faint blur, not clipped but low alpha.
    let (_, _, _, corner_alpha) = pixel(&pixmap, 0, 0);
    assert!(
        corner_alpha < 64,
        "expected distant blur to be faint; alpha at (0,0) was {corner_alpha}"
    );
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
    list.push(DisplayItem::PushBlendMode(BlendModeItem { mode: BlendMode::Color }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::RED,
    }));
    list.push(DisplayItem::PopBlendMode);

    let pixmap = renderer.render(&list).unwrap();
    let (r, g, b, a) = pixel(&pixmap, 0, 0);
    assert_eq!(a, 255);
    let expected = blend_color((255, 0, 0), (128, 128, 128));
    let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
    assert_hsl_components((r, g, b), (eh, es, el), 0.02, 0.05, 0.05, "color blend");
}

#[test]
fn hue_blend_mode_uses_source_hue() {
    use fastrender::paint::display_list::{BlendMode, BlendModeItem};

    let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let dst = (30u8, 120u8, 220u8);
    let src = (200u8, 30u8, 30u8);
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
    }));
    list.push(DisplayItem::PushBlendMode(BlendModeItem { mode: BlendMode::Hue }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
    }));
    list.push(DisplayItem::PopBlendMode);

    let pixmap = renderer.render(&list).unwrap();
    let (r, g, b, _) = pixel(&pixmap, 0, 0);
    let expected = blend_hue(src, dst);
    let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
    assert_hsl_components((r, g, b), (eh, es, el), 0.02, 0.05, 0.05, "hue blend");
}

#[test]
fn saturation_blend_mode_uses_source_saturation() {
    use fastrender::paint::display_list::{BlendMode, BlendModeItem};

    let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let dst = (60u8, 140u8, 200u8);
    let src = (255u8, 40u8, 200u8);
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
    }));
    list.push(DisplayItem::PushBlendMode(BlendModeItem {
        mode: BlendMode::Saturation,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
    }));
    list.push(DisplayItem::PopBlendMode);

    let pixmap = renderer.render(&list).unwrap();
    let (r, g, b, _) = pixel(&pixmap, 0, 0);
    let expected = blend_saturation(src, dst);
    let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
    assert_hsl_components((r, g, b), (eh, es, el), 0.02, 0.05, 0.05, "saturation blend");
}

#[test]
fn plus_lighter_blend_adds_colors() {
    use fastrender::paint::display_list::{BlendMode, BlendModeItem};

    let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(100, 100, 100, 255),
    }));
    list.push(DisplayItem::PushBlendMode(BlendModeItem {
        mode: BlendMode::PlusLighter,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(200, 0, 0, 255),
    }));
    list.push(DisplayItem::PopBlendMode);

    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixel(&pixmap, 0, 0), (255, 100, 100, 255));
}

#[test]
fn renderer_respects_device_scale() {
    let mut list = DisplayList::new();
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
        color: Rgba::rgb(255, 0, 0),
    }));

    let renderer = DisplayListRenderer::new_scaled(2, 1, Rgba::WHITE, FontContext::new(), 2.0).unwrap();
    let pixmap = renderer.render(&list).unwrap();
    assert_eq!(pixmap.width(), 4);
    assert_eq!(pixmap.height(), 2);

    let red_pixels = pixmap
        .pixels()
        .iter()
        .filter(|p| p.red() == 255 && p.green() == 0 && p.blue() == 0 && p.alpha() == 255)
        .count();
    assert!(
        red_pixels >= 4,
        "expected at least a 2x2 red block after scaling, got {}",
        red_pixels
    );
}

#[test]
fn luminosity_blend_mode_uses_source_luminance() {
    use fastrender::paint::display_list::{BlendMode, BlendModeItem};

    let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    let dst = (30u8, 200u8, 60u8);
    let src = (220u8, 40u8, 40u8);
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
    }));
    list.push(DisplayItem::PushBlendMode(BlendModeItem {
        mode: BlendMode::Luminosity,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
        color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
    }));
    list.push(DisplayItem::PopBlendMode);

    let pixmap = renderer.render(&list).unwrap();
    let (r, g, b, _) = pixel(&pixmap, 0, 0);
    let expected = blend_luminosity(src, dst);
    let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
    assert_hsl_components((r, g, b), (eh, es, el), 0.02, 0.05, 0.05, "luminosity blend");
}

#[test]
fn backdrop_blur_samples_outside_bounds() {
    let renderer = DisplayListRenderer::new(6, 1, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    // Backdrop: blue on the left, red on the right.
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(0.0, 0.0, 3.0, 1.0),
        color: Rgba::BLUE,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(3.0, 0.0, 3.0, 1.0),
        color: Rgba::RED,
    }));
    // Apply a backdrop blur over the red half; the blur should pull in blue from outside the bounds.
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(3.0, 0.0, 3.0, 1.0),
        mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
        is_isolated: false,
        transform: None,
        filters: Vec::new(),
        backdrop_filters: vec![ResolvedFilter::Blur(1.0)],
        radii: fastrender::paint::display_list::BorderRadii::ZERO,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Pixel just inside the blurred region should pick up some blue from the left half.
    let (r, g, b, a) = pixel(&pixmap, 3, 0);
    assert_eq!(a, 255);
    assert!(
        b > 0 && r < 255,
        "expected blur to sample blue neighbor; got ({r},{g},{b})"
    );
}

#[test]
fn filter_blur_not_clipped_to_bounds() {
    let renderer = DisplayListRenderer::new(4, 1, Rgba::WHITE, FontContext::new()).unwrap();
    let mut list = DisplayList::new();
    list.push(DisplayItem::PushStackingContext(StackingContextItem {
        z_index: 0,
        creates_stacking_context: true,
        bounds: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
        mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
        is_isolated: true,
        transform: None,
        filters: vec![ResolvedFilter::Blur(1.0)],
        backdrop_filters: Vec::new(),
        radii: fastrender::paint::display_list::BorderRadii::ZERO,
    }));
    list.push(DisplayItem::FillRect(FillRectItem {
        rect: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
        color: Rgba::RED,
    }));
    list.push(DisplayItem::PopStackingContext);

    let pixmap = renderer.render(&list).unwrap();
    // Blur should leak outside the original rect into pixel 0.
    let (r, g, b, a) = pixel(&pixmap, 0, 0);
    assert_eq!(a, 255);
    assert!(
        r > g && r > b && (g < 250 || b < 250),
        "expected red-dominant blur outside bounds; got ({r},{g},{b})"
    );
}

#[test]
fn color_mix_background_renders_purple() {
    let mut style = fastrender::ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
            angle: 0.0,
            stops: vec![ColorStop {
                color: fastrender::Color::parse("color-mix(in srgb, red 50%, blue 50%)").unwrap(),
                position: Some(0.0),
            }],
        }),
        ..Default::default()
    }]);

    let fragment = FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), vec![], Arc::new(style));

    let list = DisplayListBuilder::new().build(&fragment);
    let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
    let pixmap = renderer.render(&list).expect("render");

    let (r, g, b, _) = pixel(&pixmap, 2, 2);
    assert!(
        r > 0 && b > 0 && (r as i32 - b as i32).abs() <= 1,
        "expected purple mix, got ({r},{g},{b})"
    );
    assert_eq!(g, 0);
}
