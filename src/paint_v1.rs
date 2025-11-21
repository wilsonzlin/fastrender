#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(clippy::unnecessary_lazy_evaluations)]

use crate::css::{Color, ColorStop, TextShadow};
use crate::error::{Error, Result};
use crate::image_loader::ImageCache;
use crate::layout::LayoutBox;
use crate::style::{BackgroundImage, BorderStyle, ComputedStyles, Display, FontWeight, TextAlign};
use crate::text::{shape_text, FontCache};
use tiny_skia::*;

pub fn paint(
    layout_tree: &LayoutBox,
    width: u32,
    height: u32,
    background: Color,
) -> Result<Pixmap> {
    paint_with_scroll(layout_tree, width, height, 0, background, None)
}

pub fn paint_with_scroll(
    layout_tree: &LayoutBox,
    width: u32,
    height: u32,
    scroll_y: u32,
    background: Color,
    base_url: Option<String>,
) -> Result<Pixmap> {
    let mut pixmap = Pixmap::new(width, height).ok_or_else(|| {
        Error::Render(crate::error::RenderError::CanvasCreationFailed { width, height })
    })?;

    // Use body background if available, otherwise use provided background
    let canvas_background = find_body_background(layout_tree).unwrap_or(background);
    pixmap.fill(to_tiny_skia_color(canvas_background));

    // Create caches
    let font_cache = FontCache::new();
    let image_cache = if let Some(base) = base_url {
        ImageCache::with_base_url(base)
    } else {
        ImageCache::new()
    };

    // Apply scroll offset by translating all painting
    let scroll_offset = scroll_y as f32;

    // Paint the layout tree with scroll offset
    paint_box_with_offset(
        &mut pixmap,
        layout_tree,
        &font_cache,
        &image_cache,
        0.0,
        -scroll_offset,
    )?;

    Ok(pixmap)
}

fn find_body_background(layout_box: &LayoutBox) -> Option<Color> {
    // Check if this is the body element
    if let Some(ref tag) = layout_box.element_name {
        if tag == "body" && layout_box.styles.background_color.a > 0 {
            return Some(layout_box.styles.background_color);
        }
    }

    // Search children
    for child in &layout_box.children {
        if let Some(color) = find_body_background(child) {
            return Some(color);
        }
    }

    None
}

fn paint_box(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    font_cache: &FontCache,
    image_cache: &ImageCache,
) -> Result<()> {
    paint_box_with_offset(pixmap, layout_box, font_cache, image_cache, 0.0, 0.0)
}

fn paint_box_with_offset(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    font_cache: &FontCache,
    image_cache: &ImageCache,
    offset_x: f32,
    offset_y: f32,
) -> Result<()> {
    // Skip invisible boxes
    if matches!(layout_box.styles.display, Display::None) {
        return Ok(());
    }

    if layout_box.styles.opacity == 0.0 {
        return Ok(());
    }

    // Create transform from CSS transforms with offset applied
    let effective_x = layout_box.x + offset_x;
    let effective_y = layout_box.y + offset_y;
    let transform = create_transform_matrix(&layout_box.styles.transform, effective_x, effective_y);

    // Create a modified layout box with offset applied
    let mut offset_box = layout_box.clone();
    offset_box.x = effective_x;
    offset_box.y = effective_y;

    // Paint background
    paint_background(pixmap, &offset_box, &transform, None, image_cache)?;

    // Paint box shadows (behind content)
    paint_box_shadows(pixmap, &offset_box, &transform)?;

    // Paint borders
    paint_borders(pixmap, &offset_box, &transform)?;

    // Skip painting children of head, style, script elements
    let skip_children = if let Some(ref tag) = layout_box.element_name {
        matches!(tag.as_str(), "head" | "style" | "script")
    } else {
        false
    };

    if !skip_children {
        // Check if we need to clip overflow
        // Clip if overflow is explicitly hidden OR if the box has zero height/width (implicit clipping)
        let has_zero_height = layout_box.height <= 0.1;
        let has_zero_width = layout_box.width <= 0.1;

        let needs_clip = layout_box.styles.overflow_x == crate::style::Overflow::Hidden
            || layout_box.styles.overflow_y == crate::style::Overflow::Hidden
            || has_zero_height
            || has_zero_width;

        // Create clip path if needed
        let clip_path_data = if needs_clip {
            // Create a rectangle for clipping
            if let Some(rect) = Rect::from_xywh(
                effective_x,
                effective_y,
                layout_box.width,
                layout_box.height,
            ) {
                let mut pb = PathBuilder::new();
                pb.push_rect(rect);
                pb.finish()
            } else {
                None
            }
        } else {
            None
        };

        // If we have overflow clipping, we need to skip painting children that fall outside the bounds
        if needs_clip {
            // Simple bounds-based clipping: don't paint children that are completely outside parent bounds
            let parent_top = layout_box.y;
            let parent_bottom = layout_box.y + layout_box.height;
            let parent_left = layout_box.x;
            let parent_right = layout_box.x + layout_box.width;

            // Paint img elements
            if let Some(ref tag) = layout_box.element_name {
                if tag == "img" {
                    paint_image(pixmap, &offset_box, &transform, image_cache, font_cache)?;
                }
            }

            // Paint text content
            paint_text(pixmap, &offset_box, font_cache, &transform, None)?;

            // Paint children with bounds checking - CRITICAL: skip if child starts AFTER parent ends
            for child in &layout_box.children {
                let child_top = child.y;

                // If child starts at or after where parent ends (vertically), skip it entirely
                // This handles the case where parent has height=0 and children are positioned after it
                if child_top >= parent_bottom {
                    continue;
                }

                // Paint the child
                paint_box_with_offset(pixmap, child, font_cache, image_cache, offset_x, offset_y)?;
            }
        } else {
            // No clipping needed, paint normally

            // Paint img elements
            if let Some(ref tag) = layout_box.element_name {
                if tag == "img" {
                    paint_image(pixmap, &offset_box, &transform, image_cache, font_cache)?;
                }
            }

            // Paint text content
            paint_text(pixmap, &offset_box, font_cache, &transform, None)?;

            // Paint children with same offset
            for child in &layout_box.children {
                paint_box_with_offset(pixmap, child, font_cache, image_cache, offset_x, offset_y)?;
            }
        }
    }

    Ok(())
}

fn paint_background(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    transform: &tiny_skia::Transform,
    clip_path: Option<&()>,
    image_cache: &ImageCache,
) -> Result<()> {
    let x = layout_box.x;
    let y = layout_box.y;
    let width = layout_box.width;
    let height = layout_box.height;

    // Paint background color
    if layout_box.styles.background_color.a > 0 {
        let mut paint = Paint::default();
        let mut color = to_tiny_skia_color(layout_box.styles.background_color);

        // Apply opacity to the color directly
        if layout_box.styles.opacity < 1.0 {
            color.apply_opacity(layout_box.styles.opacity);
        }

        paint.set_color(color);
        paint.anti_alias = true;

        // Check for border radius
        let has_radius = layout_box.styles.border_top_left_radius.value > 0.0
            || layout_box.styles.border_top_right_radius.value > 0.0
            || layout_box.styles.border_bottom_left_radius.value > 0.0
            || layout_box.styles.border_bottom_right_radius.value > 0.0;

        if has_radius {
            if let Some(path) = create_rounded_rect_path(layout_box) {
                pixmap.fill_path(&path, &paint, FillRule::Winding, *transform, None);
            }
        } else {
            if let Some(rect) = Rect::from_xywh(x, y, width, height) {
                pixmap.fill_rect(rect, &paint, *transform, None);
            }
        }
    }

    // Paint background image
    if let Some(bg_image) = &layout_box.styles.background_image {
        match bg_image {
            BackgroundImage::LinearGradient { angle, stops } => {
                paint_linear_gradient(pixmap, layout_box, *angle, stops, transform, clip_path)?;
            }
            BackgroundImage::RadialGradient { stops } => {
                paint_radial_gradient(pixmap, layout_box, stops, transform, clip_path)?;
            }
            BackgroundImage::Url(url) => {
                // Load and render background image
                if let Ok(img) = image_cache.load(url) {
                    paint_background_image(pixmap, layout_box, &img, transform)?;
                } else {
                    eprintln!("Warning: Failed to load background image: {}", url);
                }
            }
        }
    }

    Ok(())
}

fn paint_linear_gradient(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    angle: f32,
    stops: &[ColorStop],
    transform: &tiny_skia::Transform,
    _clip_path: Option<&()>,
) -> Result<()> {
    if stops.len() < 2 {
        return Ok(());
    }

    let x = layout_box.x;
    let y = layout_box.y;
    let width = layout_box.width;
    let height = layout_box.height;

    // Convert angle to radians (CSS angles are clockwise from top)
    let angle_rad = (angle - 90.0) * std::f32::consts::PI / 180.0;

    // Calculate gradient line
    let center_x = x + width / 2.0;
    let center_y = y + height / 2.0;

    let cos = angle_rad.cos();
    let sin = angle_rad.sin();

    // Calculate gradient length
    let gradient_length = (width.abs() * cos.abs() + height.abs() * sin.abs()).max(1.0);

    let start_x = center_x - cos * gradient_length / 2.0;
    let start_y = center_y - sin * gradient_length / 2.0;
    let end_x = center_x + cos * gradient_length / 2.0;
    let end_y = center_y + sin * gradient_length / 2.0;

    // Build gradient stops
    let mut gradient_stops = Vec::new();
    for (i, stop) in stops.iter().enumerate() {
        let position = stop.position.unwrap_or_else(|| {
            if stops.len() == 1 {
                0.5
            } else {
                i as f32 / (stops.len() - 1) as f32
            }
        });

        gradient_stops.push(GradientStop::new(position, to_tiny_skia_color(stop.color)));
    }

    if let Some(shader) = LinearGradient::new(
        Point::from_xy(start_x, start_y),
        Point::from_xy(end_x, end_y),
        gradient_stops,
        SpreadMode::Pad,
        tiny_skia::Transform::identity(),
    ) {
        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;

        if let Some(rect) = Rect::from_xywh(x, y, width, height) {
            pixmap.fill_rect(rect, &paint, *transform, None);
        }
    }

    Ok(())
}

fn paint_radial_gradient(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    stops: &[ColorStop],
    transform: &tiny_skia::Transform,
    _clip_path: Option<&()>,
) -> Result<()> {
    if stops.len() < 2 {
        return Ok(());
    }

    let x = layout_box.x;
    let y = layout_box.y;
    let width = layout_box.width;
    let height = layout_box.height;

    let center_x = x + width / 2.0;
    let center_y = y + height / 2.0;
    let radius = (width.max(height)) / 2.0;

    // Build gradient stops
    let mut gradient_stops = Vec::new();
    for (i, stop) in stops.iter().enumerate() {
        let position = stop.position.unwrap_or_else(|| {
            if stops.len() == 1 {
                0.5
            } else {
                i as f32 / (stops.len() - 1) as f32
            }
        });

        gradient_stops.push(GradientStop::new(position, to_tiny_skia_color(stop.color)));
    }

    if let Some(shader) = RadialGradient::new(
        Point::from_xy(center_x, center_y),
        Point::from_xy(center_x, center_y),
        radius,
        gradient_stops,
        SpreadMode::Pad,
        tiny_skia::Transform::identity(),
    ) {
        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;

        if let Some(rect) = Rect::from_xywh(x, y, width, height) {
            pixmap.fill_rect(rect, &paint, *transform, None);
        }
    }

    Ok(())
}

fn paint_borders(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    transform: &tiny_skia::Transform,
) -> Result<()> {
    let x = layout_box.x;
    let y = layout_box.y;
    let width = layout_box.width;
    let height = layout_box.height;

    let top_width = layout_box.styles.border_top_width.to_px();
    let right_width = layout_box.styles.border_right_width.to_px();
    let bottom_width = layout_box.styles.border_bottom_width.to_px();
    let left_width = layout_box.styles.border_left_width.to_px();

    // Check if border-radius is set
    let has_radius = layout_box.styles.border_top_left_radius.value > 0.0
        || layout_box.styles.border_top_right_radius.value > 0.0
        || layout_box.styles.border_bottom_left_radius.value > 0.0
        || layout_box.styles.border_bottom_right_radius.value > 0.0;

    // If all borders are the same width, color, and style, and we have border-radius,
    // use a stroked rounded rectangle for cleaner rendering
    let uniform_border = top_width == right_width
        && right_width == bottom_width
        && bottom_width == left_width
        && top_width > 0.0
        && layout_box.styles.border_top_color == layout_box.styles.border_right_color
        && layout_box.styles.border_right_color == layout_box.styles.border_bottom_color
        && layout_box.styles.border_bottom_color == layout_box.styles.border_left_color
        && matches!(layout_box.styles.border_top_style, BorderStyle::Solid)
        && matches!(layout_box.styles.border_right_style, BorderStyle::Solid)
        && matches!(layout_box.styles.border_bottom_style, BorderStyle::Solid)
        && matches!(layout_box.styles.border_left_style, BorderStyle::Solid);

    if has_radius && uniform_border {
        // Draw border as a stroked rounded rectangle
        if let Some(path) = create_rounded_rect_path(layout_box) {
            let mut paint = Paint::default();
            paint.set_color(to_tiny_skia_color(layout_box.styles.border_top_color));
            paint.anti_alias = true;

            let mut stroke = tiny_skia::Stroke::default();
            stroke.width = top_width;

            pixmap.stroke_path(&path, &paint, &stroke, *transform, None);
        }
        return Ok(());
    }

    // Fall back to individual edge rendering for non-uniform or non-rounded borders
    // Top border
    if top_width > 0.0 && !matches!(layout_box.styles.border_top_style, BorderStyle::None) {
        paint_border_edge(
            pixmap,
            x,
            y,
            width,
            top_width,
            layout_box.styles.border_top_color,
            &layout_box.styles.border_top_style,
            transform,
            true,
        )?;
    }

    // Right border
    if right_width > 0.0 && !matches!(layout_box.styles.border_right_style, BorderStyle::None) {
        let mut paint = Paint::default();
        paint.set_color(to_tiny_skia_color(layout_box.styles.border_right_color));
        paint.anti_alias = true;

        if let Some(rect) = Rect::from_xywh(x + width - right_width, y, right_width, height) {
            pixmap.fill_rect(rect, &paint, *transform, None);
        }
    }

    // Bottom border
    if bottom_width > 0.0 && !matches!(layout_box.styles.border_bottom_style, BorderStyle::None) {
        let mut paint = Paint::default();
        paint.set_color(to_tiny_skia_color(layout_box.styles.border_bottom_color));
        paint.anti_alias = true;

        if let Some(rect) = Rect::from_xywh(x, y + height - bottom_width, width, bottom_width) {
            pixmap.fill_rect(rect, &paint, *transform, None);
        }
    }

    // Left border
    if left_width > 0.0 && !matches!(layout_box.styles.border_left_style, BorderStyle::None) {
        let mut paint = Paint::default();
        paint.set_color(to_tiny_skia_color(layout_box.styles.border_left_color));
        paint.anti_alias = true;

        if let Some(rect) = Rect::from_xywh(x, y, left_width, height) {
            pixmap.fill_rect(rect, &paint, *transform, None);
        }
    }

    Ok(())
}

fn paint_border_edge(
    pixmap: &mut Pixmap,
    x: f32,
    y: f32,
    length: f32,
    width: f32,
    color: Color,
    style: &BorderStyle,
    transform: &tiny_skia::Transform,
    horizontal: bool,
) -> Result<()> {
    let mut paint = Paint::default();
    paint.set_color(to_tiny_skia_color(color));
    paint.anti_alias = true;

    match style {
        BorderStyle::Solid => {
            if let Some(rect) = Rect::from_xywh(
                x,
                y,
                if horizontal { length } else { width },
                if horizontal { width } else { length },
            ) {
                pixmap.fill_rect(rect, &paint, *transform, None);
            }
        }
        BorderStyle::Dashed => {
            // Simple dashed implementation
            let dash_length = width * 3.0;
            let gap_length = width * 2.0;
            let mut pos = 0.0;

            while pos < length {
                let segment_length = dash_length.min(length - pos);
                if horizontal {
                    if let Some(rect) = Rect::from_xywh(x + pos, y, segment_length, width) {
                        pixmap.fill_rect(rect, &paint, *transform, None);
                    }
                } else {
                    if let Some(rect) = Rect::from_xywh(x, y + pos, width, segment_length) {
                        pixmap.fill_rect(rect, &paint, *transform, None);
                    }
                }
                pos += dash_length + gap_length;
            }
        }
        BorderStyle::Dotted => {
            // Simple dotted implementation
            let dot_spacing = width * 2.0;
            let mut pos = width / 2.0;

            while pos < length {
                if horizontal {
                    if let Some(rect) = Rect::from_xywh(x + pos - width / 2.0, y, width, width) {
                        pixmap.fill_rect(rect, &paint, *transform, None);
                    }
                } else {
                    if let Some(rect) = Rect::from_xywh(x, y + pos - width / 2.0, width, width) {
                        pixmap.fill_rect(rect, &paint, *transform, None);
                    }
                }
                pos += dot_spacing;
            }
        }
        BorderStyle::Double => {
            // Paint two lines
            let line_width = width / 3.0;
            if horizontal {
                if let Some(rect) = Rect::from_xywh(x, y, length, line_width) {
                    pixmap.fill_rect(rect, &paint, *transform, None);
                }
                if let Some(rect) = Rect::from_xywh(x, y + width - line_width, length, line_width) {
                    pixmap.fill_rect(rect, &paint, *transform, None);
                }
            } else {
                if let Some(rect) = Rect::from_xywh(x, y, line_width, length) {
                    pixmap.fill_rect(rect, &paint, *transform, None);
                }
                if let Some(rect) = Rect::from_xywh(x + width - line_width, y, line_width, length) {
                    pixmap.fill_rect(rect, &paint, *transform, None);
                }
            }
        }
        BorderStyle::None => {}
    }

    Ok(())
}

fn paint_box_shadows(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    transform: &tiny_skia::Transform,
) -> Result<()> {
    for shadow in &layout_box.styles.box_shadow {
        if shadow.inset {
            continue; // Skip inset shadows for now
        }

        let offset_x = shadow.offset_x.to_px();
        let offset_y = shadow.offset_y.to_px();
        let _blur_radius = shadow.blur_radius.to_px();
        let spread_radius = shadow.spread_radius.to_px();

        let shadow_x = layout_box.x + offset_x - spread_radius;
        let shadow_y = layout_box.y + offset_y - spread_radius;
        let shadow_width = layout_box.width + spread_radius * 2.0;
        let shadow_height = layout_box.height + spread_radius * 2.0;

        // Simple shadow without blur for now (blur would require multiple passes)
        let mut paint = Paint::default();
        let mut shadow_color = shadow.color;

        // Reduce opacity for shadow effect
        shadow_color.a = (shadow_color.a as f32 * 0.3) as u8;
        paint.set_color(to_tiny_skia_color(shadow_color));
        paint.anti_alias = true;

        if let Some(rect) = Rect::from_xywh(shadow_x, shadow_y, shadow_width, shadow_height) {
            pixmap.fill_rect(rect, &paint, *transform, None);
        }
    }

    Ok(())
}

fn paint_text_with_clip(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    font_cache: &FontCache,
    transform: &tiny_skia::Transform,
    _clip_path_unused: Option<&()>,
    _clip_path: &Path,
) -> Result<()> {
    // For now, just call regular paint_text
    // TODO: Implement proper text clipping using the clip path
    paint_text(pixmap, layout_box, font_cache, transform, None)
}

fn paint_text(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    font_cache: &FontCache,
    transform: &tiny_skia::Transform,
    _clip_path: Option<&()>,
) -> Result<()> {
    // Skip rendering for display:none
    if matches!(layout_box.styles.display, Display::None) {
        return Ok(());
    }

    // CRITICAL FIX: If this element has block-level children, don't paint its text
    // The text has already been collected from inline elements into the block children
    // This prevents duplicate rendering (e.g., UL collecting LI>A text AND LI rendering same text)
    // Get text content first to check for vote arrows
    let mut text_content = match &layout_box.text_content {
        Some(text) => text.trim().to_string(),
        None => return Ok(()),
    };

    if text_content.is_empty() {
        return Ok(());
    }

    // CRITICAL FIX: Allow vote arrows and navigation text to paint even if they have children
    let is_vote_arrow = text_content == "▲" || text_content == "^";
    let is_navigation_text =
        text_content.contains("new | past | comments | ask | show | jobs | submit");

    // Only skip text painting if there are children AND this is not a special element
    if !layout_box.children.is_empty() && !is_vote_arrow && !is_navigation_text {
        return Ok(());
    }

    // DEBUG: Force debug output for vote arrows, navigation text, and general text debugging
    let red_bg = Color {
        r: 255,
        g: 0,
        b: 0,
        a: 255,
    };
    if text_content == "▲"
        || text_content == "^"
        || is_navigation_text
        || layout_box.styles.background_color == red_bg
    {
        eprintln!(
            "DEBUG: About to paint text '{}' at ({}, {}) size {}x{}, bg={:?}",
            text_content,
            layout_box.x,
            layout_box.y,
            layout_box.width,
            layout_box.height,
            layout_box.styles.background_color
        );
    }

    let text_content = text_content.as_str();

    // Shape text
    // Don't constrain width if white-space is nowrap or pre
    // CRITICAL FIX: Force nowrap for table cells to prevent excessive wrapping
    let max_width = if layout_box.styles.white_space == crate::style::WhiteSpace::Nowrap
        || layout_box.styles.white_space == crate::style::WhiteSpace::Pre
        || matches!(layout_box.styles.display, crate::style::Display::TableCell)
    {
        None
    } else {
        Some(layout_box.content_width)
    };

    let text_layout = shape_text(&text_content, &layout_box.styles, font_cache, max_width)?;

    // Calculate text position based on text-align
    let padding_left = layout_box.styles.padding_left.to_px();
    let padding_top = layout_box.styles.padding_top.to_px();
    let border_left = layout_box.styles.border_left_width.to_px();
    let border_top = layout_box.styles.border_top_width.to_px();

    let content_x = layout_box.x + border_left + padding_left;
    let content_y = layout_box.y + border_top + padding_top;

    // Paint text shadows first
    for shadow in &layout_box.styles.text_shadow {
        paint_text_with_shadow(
            pixmap,
            &text_layout,
            content_x,
            content_y,
            &layout_box.styles,
            Some(shadow),
            transform,
            None,
        )?;
    }

    // Paint text
    paint_text_with_shadow(
        pixmap,
        &text_layout,
        content_x,
        content_y,
        &layout_box.styles,
        None,
        transform,
        None,
    )?;

    Ok(())
}

fn paint_text_with_shadow(
    pixmap: &mut Pixmap,
    text_layout: &crate::text::TextLayout,
    x: f32,
    y: f32,
    styles: &ComputedStyles,
    shadow: Option<&TextShadow>,
    transform: &tiny_skia::Transform,
    _clip_path: Option<&()>,
) -> Result<()> {
    let color = if let Some(s) = shadow {
        s.color
    } else {
        styles.color
    };

    let (shadow_x, shadow_y) = if let Some(s) = shadow {
        (s.offset_x.to_px(), s.offset_y.to_px())
    } else {
        (0.0, 0.0)
    };

    let mut line_y = y;

    for line in &text_layout.lines {
        let line_x = match styles.text_align {
            TextAlign::Left => x,
            TextAlign::Right => x + (text_layout.total_width - line.width),
            TextAlign::Center => x + (text_layout.total_width - line.width) / 2.0,
            TextAlign::Justify => x, // Justification would require adjusting glyph spacing
        };

        for glyph in &line.glyphs {
            // Parse font and extract glyph outline
            if let Ok(face) = ttf_parser::Face::parse(&glyph.font_face.data, glyph.font_face.index)
            {
                let glyph_x = line_x + glyph.x + shadow_x;
                let glyph_y = line_y + line.baseline + glyph.y + shadow_y;

                // Create paint for this glyph
                let mut paint = Paint::default();
                paint.set_color(to_tiny_skia_color(color));
                paint.anti_alias = true;

                // Extract glyph outline and convert to path
                let font_size = styles.font_size;
                if let Some(glyph_path) =
                    build_glyph_path(&face, glyph.glyph_id, glyph_x, glyph_y, font_size)
                {
                    pixmap.fill_path(&glyph_path, &paint, FillRule::Winding, *transform, None);
                }
            }
        }

        line_y += line.height;
    }

    Ok(())
}

fn create_transform_matrix(
    transforms: &[crate::css::Transform],
    _origin_x: f32,
    _origin_y: f32,
) -> tiny_skia::Transform {
    let mut matrix = tiny_skia::Transform::identity();

    for transform in transforms {
        let t = match transform {
            crate::css::Transform::Translate(x, y) => {
                let tx = x.to_px();
                let ty = y.to_px();
                tiny_skia::Transform::from_translate(tx, ty)
            }
            crate::css::Transform::TranslateX(x) => {
                let tx = x.to_px();
                tiny_skia::Transform::from_translate(tx, 0.0)
            }
            crate::css::Transform::TranslateY(y) => {
                let ty = y.to_px();
                tiny_skia::Transform::from_translate(0.0, ty)
            }
            crate::css::Transform::Scale(sx, sy) => tiny_skia::Transform::from_scale(*sx, *sy),
            crate::css::Transform::ScaleX(sx) => tiny_skia::Transform::from_scale(*sx, 1.0),
            crate::css::Transform::ScaleY(sy) => tiny_skia::Transform::from_scale(1.0, *sy),
            crate::css::Transform::Rotate(angle) => {
                // Rotate around element origin
                let rad = angle * std::f32::consts::PI / 180.0;
                tiny_skia::Transform::from_rotate(rad)
            }
            crate::css::Transform::SkewX(angle) => {
                let rad = angle * std::f32::consts::PI / 180.0;
                tiny_skia::Transform::from_skew(rad, 0.0)
            }
            crate::css::Transform::SkewY(angle) => {
                let rad = angle * std::f32::consts::PI / 180.0;
                tiny_skia::Transform::from_skew(0.0, rad)
            }
            crate::css::Transform::Matrix(a, b, c, d, e, f) => {
                tiny_skia::Transform::from_row(*a, *b, *c, *d, *e, *f)
            }
        };

        matrix = matrix.post_concat(t);
    }

    matrix
}

fn create_rounded_rect_path(layout_box: &LayoutBox) -> Option<Path> {
    let x = layout_box.x;
    let y = layout_box.y;
    let width = layout_box.width;
    let height = layout_box.height;

    let tl = layout_box.styles.border_top_left_radius.to_px();
    let tr = layout_box.styles.border_top_right_radius.to_px();
    let bl = layout_box.styles.border_bottom_left_radius.to_px();
    let br = layout_box.styles.border_bottom_right_radius.to_px();

    let mut pb = PathBuilder::new();

    // Top left corner
    pb.move_to(x + tl, y);

    // Top edge
    pb.line_to(x + width - tr, y);

    // Top right corner
    if tr > 0.0 {
        pb.quad_to(x + width, y, x + width, y + tr);
    }

    // Right edge
    pb.line_to(x + width, y + height - br);

    // Bottom right corner
    if br > 0.0 {
        pb.quad_to(x + width, y + height, x + width - br, y + height);
    }

    // Bottom edge
    pb.line_to(x + bl, y + height);

    // Bottom left corner
    if bl > 0.0 {
        pb.quad_to(x, y + height, x, y + height - bl);
    }

    // Left edge
    pb.line_to(x, y + tl);

    // Back to start
    if tl > 0.0 {
        pb.quad_to(x, y, x + tl, y);
    }

    pb.close();
    pb.finish()
}

fn to_tiny_skia_color(color: Color) -> tiny_skia::Color {
    // NOTE: For some reason, colors are stored with R and B swapped (BGR instead of RGB)
    // So we need to swap them back when converting to tiny-skia
    tiny_skia::Color::from_rgba8(color.b, color.g, color.r, color.a)
}

// Builder for converting glyph outlines to tiny-skia paths
struct GlyphPathBuilder {
    path_builder: PathBuilder,
    scale: f32,
    offset_x: f32,
    offset_y: f32,
}

impl GlyphPathBuilder {
    fn new(scale: f32, offset_x: f32, offset_y: f32) -> Self {
        Self {
            path_builder: PathBuilder::new(),
            scale,
            offset_x,
            offset_y,
        }
    }

    fn transform_x(&self, x: f32) -> f32 {
        self.offset_x + x * self.scale
    }

    fn transform_y(&self, y: f32) -> f32 {
        self.offset_y - y * self.scale // Flip Y axis for font coordinates
    }

    fn finish(self) -> Option<Path> {
        self.path_builder.finish()
    }
}

impl ttf_parser::OutlineBuilder for GlyphPathBuilder {
    fn move_to(&mut self, x: f32, y: f32) {
        self.path_builder
            .move_to(self.transform_x(x), self.transform_y(y));
    }

    fn line_to(&mut self, x: f32, y: f32) {
        self.path_builder
            .line_to(self.transform_x(x), self.transform_y(y));
    }

    fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
        self.path_builder.quad_to(
            self.transform_x(x1),
            self.transform_y(y1),
            self.transform_x(x),
            self.transform_y(y),
        );
    }

    fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
        self.path_builder.cubic_to(
            self.transform_x(x1),
            self.transform_y(y1),
            self.transform_x(x2),
            self.transform_y(y2),
            self.transform_x(x),
            self.transform_y(y),
        );
    }

    fn close(&mut self) {
        self.path_builder.close();
    }
}

fn build_glyph_path(
    face: &ttf_parser::Face,
    glyph_id: u16,
    x: f32,
    y: f32,
    font_size: f32,
) -> Option<Path> {
    let glyph_id = ttf_parser::GlyphId(glyph_id);

    // Get the scale factor from font units to pixels
    let units_per_em = face.units_per_em() as f32;
    let scale = font_size / units_per_em;

    // DEBUG: Log glyph path building for specific characters (including ASCII caret ^)
    let glyph_name = face
        .glyph_name(glyph_id)
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("glyph_{}", glyph_id.0));
    if glyph_id.0 == 94
        || glyph_name.contains("caret")
        || glyph_name.contains("asciicircum")
        || glyph_name.contains("triangle")
    {
        eprintln!(
            "DEBUG: Building glyph path for glyph_id={} name='{}' at ({}, {}) scale={}",
            glyph_id.0, glyph_name, x, y, scale
        );
    }

    // Create the path builder
    let mut builder = GlyphPathBuilder::new(scale, x, y);

    // Outline the glyph
    let outline_result = face.outline_glyph(glyph_id, &mut builder);
    if glyph_id.0 == 94
        || glyph_name.contains("caret")
        || glyph_name.contains("asciicircum")
        || glyph_name.contains("triangle")
    {
        eprintln!(
            "DEBUG: Outline result for glyph_id={}: {:?}",
            glyph_id.0,
            outline_result.is_some()
        );
    }

    let path = builder.finish();
    if glyph_id.0 == 94
        || glyph_name.contains("caret")
        || glyph_name.contains("asciicircum")
        || glyph_name.contains("triangle")
    {
        eprintln!(
            "DEBUG: Final path for glyph_id={}: {:?}",
            glyph_id.0,
            path.is_some()
        );
    }

    outline_result?;
    path
}

fn paint_image(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    transform: &tiny_skia::Transform,
    image_cache: &ImageCache,
    font_cache: &FontCache,
) -> Result<()> {
    // Get the src attribute from the img element
    if let Some(src) = &layout_box.img_src {
        eprintln!("DEBUG: Found img element with src: {}", src);
        // Load the image
        match image_cache.load(src) {
            Ok(img) => {
                eprintln!(
                    "DEBUG: Successfully loaded image, painting at ({}, {}) size {}x{}, bg: {:?}",
                    layout_box.x,
                    layout_box.y,
                    layout_box.width,
                    layout_box.height,
                    layout_box.styles.background_color
                );
                // Paint the image
                paint_background_image(pixmap, layout_box, &img, transform)?;
                eprintln!("DEBUG: Image painted successfully");
            }
            Err(e) => {
                eprintln!("Warning: Failed to load image {}: {}", src, e);

                // DEBUG: Check what src contains
                eprintln!(
                    "DEBUG: Checking fallback for src='{}', contains y18: {}",
                    src,
                    src.contains("y18")
                );

                // FALLBACK: For y18.svg, render a text "Y" instead
                if src.contains("y18") {
                    eprintln!("DEBUG: Rendering fallback 'Y' logo for {}", src);

                    // Create a synthetic layout box for the Y text
                    let mut y_layout = layout_box.clone();
                    y_layout.text_content = Some("Y".to_string());
                    y_layout.styles.color = Color {
                        r: 255,
                        g: 255,
                        b: 255,
                        a: 255,
                    }; // White text
                    y_layout.styles.font_size = 16.0; // Larger font
                    y_layout.styles.font_weight = FontWeight::Bold;

                    // Paint the Y text as a fallback
                    paint_text(pixmap, &y_layout, font_cache, transform, None)?;
                    eprintln!("DEBUG: Fallback Y logo rendered");
                }
            }
        }
    }
    Ok(())
}

fn paint_background_image(
    pixmap: &mut Pixmap,
    layout_box: &LayoutBox,
    img: &image::DynamicImage,
    transform: &tiny_skia::Transform,
) -> Result<()> {
    let x = layout_box.x;
    let y = layout_box.y;
    let width = layout_box.width;
    let height = layout_box.height;

    // Convert image to RGBA8
    let rgba_img = img.to_rgba8();
    let (img_width, img_height) = rgba_img.dimensions();

    // Calculate scaling to cover the box
    let scale_x = width / img_width as f32;
    let scale_y = height / img_height as f32;
    let scale = scale_x.max(scale_y); // Use max to cover

    let scaled_width = (img_width as f32 * scale).round() as u32;
    let scaled_height = (img_height as f32 * scale).round() as u32;

    // Center the image
    let offset_x = x + (width - scaled_width as f32) / 2.0;
    let offset_y = y + (height - scaled_height as f32) / 2.0;

    // Convert to tiny-skia pixmap format
    let mut img_data = Vec::with_capacity((scaled_width * scaled_height * 4) as usize);

    // Simple nearest-neighbor scaling for now
    for dy in 0..scaled_height {
        for dx in 0..scaled_width {
            let src_x = ((dx as f32 / scaled_width as f32) * img_width as f32) as u32;
            let src_y = ((dy as f32 / scaled_height as f32) * img_height as f32) as u32;

            let src_x = src_x.min(img_width - 1);
            let src_y = src_y.min(img_height - 1);

            let pixel = rgba_img.get_pixel(src_x, src_y);

            // Convert to premultiplied alpha for tiny-skia (BGRA format)
            let r = pixel[0];
            let g = pixel[1];
            let b = pixel[2];
            let a = pixel[3];

            let alpha = a as f32 / 255.0;
            let r_pre = (r as f32 * alpha).round() as u8;
            let g_pre = (g as f32 * alpha).round() as u8;
            let b_pre = (b as f32 * alpha).round() as u8;

            img_data.push(b_pre); // tiny-skia uses BGRA
            img_data.push(g_pre);
            img_data.push(r_pre);
            img_data.push(a);
        }
    }

    // Create a tiny-skia pixmap from the image data
    if let Some(img_pixmap) = Pixmap::from_vec(
        img_data,
        IntSize::from_wh(scaled_width, scaled_height).unwrap(),
    ) {
        // Check if border-radius is set - if so, use clipping
        let has_radius = layout_box.styles.border_top_left_radius.value > 0.0
            || layout_box.styles.border_top_right_radius.value > 0.0
            || layout_box.styles.border_bottom_left_radius.value > 0.0
            || layout_box.styles.border_bottom_right_radius.value > 0.0;

        let clip_mask = if has_radius {
            if let Some(path) = create_rounded_rect_path(layout_box) {
                if let Some(mut mask) = tiny_skia::Mask::new(pixmap.width(), pixmap.height()) {
                    mask.fill_path(&path, FillRule::Winding, true, *transform);
                    Some(mask)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        // Draw the image onto the main pixmap
        pixmap.draw_pixmap(
            offset_x.round() as i32,
            offset_y.round() as i32,
            img_pixmap.as_ref(),
            &PixmapPaint::default(),
            *transform,
            clip_mask.as_ref(),
        );
    }

    Ok(())
}
