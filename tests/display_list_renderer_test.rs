use fastrender::geometry::Rect;
use fastrender::paint::display_list::{DisplayItem, DisplayList, FillRectItem, ResolvedFilter, StackingContextItem};
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::text::font_loader::FontContext;
use fastrender::Rgba;

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
    let s = if l > 0.5 { d / (2.0 - max - min) } else { d / (max + min) };
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
        hue_delta(h, expected_hsl.0) <= tol_h && (s - expected_hsl.1).abs() <= tol_s && (l - expected_hsl.2).abs() <= tol_l,
        "{context}: expected hsl {:?}, got hsl ({h:.3},{s:.3},{l:.3})",
        expected_hsl
    );
}

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
    list.push(DisplayItem::PushBlendMode(BlendModeItem {
        mode: BlendMode::Hue,
    }));
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
    assert!(r > g && r > b && (g < 250 || b < 250), "expected red-dominant blur outside bounds; got ({r},{g},{b})");
}
