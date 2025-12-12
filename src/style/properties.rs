//! CSS Property Application
//!
//! This module handles applying individual CSS property declarations
//! to a computed style struct.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::properties::parse_length;
use crate::css::types::{Declaration, PropertyValue};
use crate::style::color::Rgba;
use crate::style::content::{parse_content, ContentValue};
use crate::style::counters::CounterSet;
use crate::style::display::Display;
use crate::style::float::{Clear, Float};
use crate::style::grid::{
    parse_grid_shorthand, parse_grid_template_areas, parse_grid_template_shorthand, parse_grid_tracks_with_names,
    parse_track_list, ParsedTracks,
};
use crate::style::position::Position;
use crate::style::types::*;
use crate::style::values::{Length, LengthUnit};
use crate::style::var_resolution::{resolve_var_for_property, VarResolutionResult};
use crate::style::ComputedStyle;
use cssparser::{Parser, ParserInput, Token};
use std::cell::Cell;

thread_local! {
    static IMAGE_SET_DPR: Cell<f32> = Cell::new(1.0);
}

/// Executes a closure with the image-set selection density overridden.
/// Restores the previous value afterward.
pub fn with_image_set_dpr<T>(dpr: f32, f: impl FnOnce() -> T) -> T {
    IMAGE_SET_DPR.with(|cell| {
        let prev = cell.get();
        let clamped = if dpr.is_finite() && dpr > 0.0 { dpr } else { 1.0 };
        cell.set(clamped);
        let result = f();
        cell.set(prev);
        result
    })
}

fn current_image_set_dpr() -> f32 {
    IMAGE_SET_DPR.with(|cell| cell.get())
}

fn split_layers(tokens: &[PropertyValue]) -> Vec<Vec<PropertyValue>> {
    let mut layers = Vec::new();
    let mut current = Vec::new();
    for token in tokens {
        if matches!(token, PropertyValue::Keyword(k) if k == ",") {
            if !current.is_empty() {
                layers.push(current.clone());
                current.clear();
            }
        } else {
            current.push(token.clone());
        }
    }
    if !current.is_empty() {
        layers.push(current);
    }
    if layers.is_empty() {
        layers.push(Vec::new());
    }
    layers
}

fn parse_background_image_value(value: &PropertyValue) -> Option<BackgroundImage> {
    match value {
        PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => parse_image_set(kw),
        PropertyValue::Url(url) => Some(BackgroundImage::Url(url.clone())),
        PropertyValue::LinearGradient { angle, stops } => Some(BackgroundImage::LinearGradient {
            angle: *angle,
            stops: stops.clone(),
        }),
        PropertyValue::RadialGradient {
            shape,
            size,
            position,
            stops,
        } => Some(BackgroundImage::RadialGradient {
            shape: *shape,
            size: size.clone(),
            position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: position.x.alignment,
                    offset: position.x.offset,
                },
                y: BackgroundPositionComponent {
                    alignment: position.y.alignment,
                    offset: position.y.offset,
                },
            },
            stops: stops.clone(),
        }),
        PropertyValue::RepeatingLinearGradient { angle, stops } => Some(BackgroundImage::RepeatingLinearGradient {
            angle: *angle,
            stops: stops.clone(),
        }),
        PropertyValue::RepeatingRadialGradient {
            shape,
            size,
            position,
            stops,
        } => Some(BackgroundImage::RepeatingRadialGradient {
            shape: *shape,
            size: size.clone(),
            position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: position.x.alignment,
                    offset: position.x.offset,
                },
                y: BackgroundPositionComponent {
                    alignment: position.y.alignment,
                    offset: position.y.offset,
                },
            },
            stops: stops.clone(),
        }),
        PropertyValue::ConicGradient {
            from_angle,
            position,
            stops,
        } => Some(BackgroundImage::ConicGradient {
            from_angle: *from_angle,
            position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: position.x.alignment,
                    offset: position.x.offset,
                },
                y: BackgroundPositionComponent {
                    alignment: position.y.alignment,
                    offset: position.y.offset,
                },
            },
            stops: stops.clone(),
        }),
        PropertyValue::RepeatingConicGradient {
            from_angle,
            position,
            stops,
        } => Some(BackgroundImage::RepeatingConicGradient {
            from_angle: *from_angle,
            position: BackgroundPosition::Position {
                x: BackgroundPositionComponent {
                    alignment: position.x.alignment,
                    offset: position.x.offset,
                },
                y: BackgroundPositionComponent {
                    alignment: position.y.alignment,
                    offset: position.y.offset,
                },
            },
            stops: stops.clone(),
        }),
        PropertyValue::Keyword(kw) if kw == "none" => Some(BackgroundImage::None),
        _ => None,
    }
}

fn parse_image_set_resolution(token: &str) -> Option<f32> {
    let lower = token.trim().to_ascii_lowercase();
    if let Some(rest) = lower.strip_suffix("x") {
        return rest.parse::<f32>().ok().filter(|v| *v > 0.0);
    }
    if let Some(rest) = lower.strip_suffix("dppx") {
        return rest.parse::<f32>().ok().filter(|v| *v > 0.0);
    }
    if let Some(rest) = lower.strip_suffix("dpi") {
        return rest.parse::<f32>().ok().filter(|v| *v > 0.0).map(|dpi| dpi / 96.0);
    }
    if let Some(rest) = lower.strip_suffix("dpcm") {
        return rest
            .parse::<f32>()
            .ok()
            .filter(|v| *v > 0.0)
            .map(|dpcm| (dpcm * 2.54) / 96.0);
    }
    None
}

fn split_image_set_candidates(inner: &str) -> Vec<String> {
    let mut paren = 0usize;
    let mut bracket = 0usize;
    let mut brace = 0usize;
    let mut current = String::new();
    let mut parts = Vec::new();
    let mut in_string: Option<char> = None;
    let mut chars = inner.chars().peekable();

    while let Some(ch) = chars.next() {
        if let Some(quote) = in_string {
            current.push(ch);
            if ch == '\\' {
                if let Some(next) = chars.next() {
                    current.push(next);
                }
                continue;
            }
            if ch == quote {
                in_string = None;
            }
            continue;
        }

        match ch {
            '"' | '\'' => {
                in_string = Some(ch);
                current.push(ch);
            }
            '(' => {
                paren += 1;
                current.push(ch);
            }
            ')' => {
                if paren > 0 {
                    paren -= 1;
                }
                current.push(ch);
            }
            '[' => {
                bracket += 1;
                current.push(ch);
            }
            ']' => {
                if bracket > 0 {
                    bracket -= 1;
                }
                current.push(ch);
            }
            '{' => {
                brace += 1;
                current.push(ch);
            }
            '}' => {
                if brace > 0 {
                    brace -= 1;
                }
                current.push(ch);
            }
            ',' if paren == 0 && bracket == 0 && brace == 0 => {
                if !current.trim().is_empty() {
                    parts.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }

    if !current.trim().is_empty() {
        parts.push(current.trim().to_string());
    }

    parts
}

pub(crate) fn parse_image_set(text: &str) -> Option<BackgroundImage> {
    let trimmed = text.trim();
    if !trimmed.to_ascii_lowercase().starts_with("image-set(") {
        return None;
    }

    let start = trimmed.find('(')?;
    let mut depth = 0usize;
    let mut end = None;
    for (idx, ch) in trimmed.char_indices().skip(start) {
        match ch {
            '(' => depth += 1,
            ')' => {
                if depth > 0 {
                    depth -= 1;
                    if depth == 0 {
                        end = Some(idx);
                        break;
                    }
                }
            }
            _ => {}
        }
    }

    let end_idx = end?;
    if !trimmed[end_idx + 1..].trim().is_empty() {
        return None;
    }
    let inner = trimmed.get(start + 1..end_idx)?.trim();
    let mut candidates = Vec::new();

    for candidate in split_image_set_candidates(inner) {
        let tokens = tokenize_image_set_candidate(&candidate);
        if tokens.is_empty() {
            continue;
        }
        let image_token = &tokens[0];
        let image = if image_token.trim().to_ascii_lowercase().starts_with("url(") && image_token.trim().ends_with(')')
        {
            let trimmed = image_token.trim();
            let open = trimmed.find('(').unwrap_or(0);
            let inner = trimmed
                .get(open + 1..trimmed.len() - 1)
                .unwrap_or("")
                .trim()
                .trim_matches(|c| c == '"' || c == '\'')
                .to_string();
            Some(BackgroundImage::Url(inner))
        } else {
            crate::css::properties::parse_property_value("background-image", image_token)
                .and_then(|prop| parse_background_image_value(&prop))
        };
        let Some(image) = image else { continue };

        let mut density = 1.0;
        for token in tokens.iter().skip(1) {
            if let Some(res) = parse_image_set_resolution(token) {
                density = res;
            }
        }
        candidates.push((image, density));
    }

    if candidates.is_empty() {
        return None;
    }

    let desired = current_image_set_dpr();
    let mut best_ge: Option<(BackgroundImage, f32)> = None;
    let mut best_lt: Option<(BackgroundImage, f32)> = None;

    for (image, density) in candidates {
        if !density.is_finite() || density <= 0.0 {
            continue;
        }
        if density >= desired {
            let replace = best_ge.as_ref().map(|(_, d)| density < *d).unwrap_or(true);
            if replace {
                best_ge = Some((image, density));
            }
        } else {
            let replace = best_lt.as_ref().map(|(_, d)| density > *d).unwrap_or(true);
            if replace {
                best_lt = Some((image, density));
            }
        }
    }

    if let Some((image, _)) = best_ge {
        Some(image)
    } else if let Some((image, _)) = best_lt {
        Some(image)
    } else {
        None
    }
}

fn tokenize_image_set_candidate(value_str: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current = String::new();
    let mut paren = 0;
    let mut bracket = 0;
    let mut brace = 0;
    let mut in_string: Option<char> = None;
    let mut chars = value_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if let Some(quote) = in_string {
            current.push(ch);
            if ch == '\\' {
                if let Some(next) = chars.next() {
                    current.push(next);
                }
                continue;
            }
            if ch == quote {
                in_string = None;
            }
            continue;
        }

        match ch {
            '"' | '\'' => {
                in_string = Some(ch);
                current.push(ch);
            }
            '(' => {
                paren += 1;
                current.push(ch);
            }
            ')' => {
                if paren > 0 {
                    paren -= 1;
                }
                current.push(ch);
            }
            '[' => {
                bracket += 1;
                current.push(ch);
            }
            ']' => {
                if bracket > 0 {
                    bracket -= 1;
                }
                current.push(ch);
            }
            '{' => {
                brace += 1;
                current.push(ch);
            }
            '}' => {
                if brace > 0 {
                    brace -= 1;
                }
                current.push(ch);
            }
            ch if ch.is_whitespace() && paren == 0 && bracket == 0 && brace == 0 => {
                if !current.trim().is_empty() {
                    tokens.push(current.trim().to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }

    if !current.trim().is_empty() {
        tokens.push(current.trim().to_string());
    }
    tokens
}

fn parse_cursor_keyword(kw: &str) -> Option<CursorKeyword> {
    match kw.to_ascii_lowercase().as_str() {
        "auto" => Some(CursorKeyword::Auto),
        "default" => Some(CursorKeyword::Default),
        "none" => Some(CursorKeyword::None),
        "context-menu" => Some(CursorKeyword::ContextMenu),
        "help" => Some(CursorKeyword::Help),
        "pointer" => Some(CursorKeyword::Pointer),
        "progress" => Some(CursorKeyword::Progress),
        "wait" => Some(CursorKeyword::Wait),
        "cell" => Some(CursorKeyword::Cell),
        "crosshair" => Some(CursorKeyword::Crosshair),
        "text" => Some(CursorKeyword::Text),
        "vertical-text" => Some(CursorKeyword::VerticalText),
        "alias" => Some(CursorKeyword::Alias),
        "copy" => Some(CursorKeyword::Copy),
        "move" => Some(CursorKeyword::Move),
        "no-drop" => Some(CursorKeyword::NoDrop),
        "not-allowed" => Some(CursorKeyword::NotAllowed),
        "grab" => Some(CursorKeyword::Grab),
        "grabbing" => Some(CursorKeyword::Grabbing),
        "all-scroll" => Some(CursorKeyword::AllScroll),
        "col-resize" => Some(CursorKeyword::ColResize),
        "row-resize" => Some(CursorKeyword::RowResize),
        "n-resize" => Some(CursorKeyword::NResize),
        "s-resize" => Some(CursorKeyword::SResize),
        "e-resize" => Some(CursorKeyword::EResize),
        "w-resize" => Some(CursorKeyword::WResize),
        "ne-resize" => Some(CursorKeyword::NeResize),
        "nw-resize" => Some(CursorKeyword::NwResize),
        "se-resize" => Some(CursorKeyword::SeResize),
        "sw-resize" => Some(CursorKeyword::SwResize),
        "ew-resize" => Some(CursorKeyword::EwResize),
        "ns-resize" => Some(CursorKeyword::NsResize),
        "nesw-resize" => Some(CursorKeyword::NeswResize),
        "nwse-resize" => Some(CursorKeyword::NwseResize),
        "zoom-in" => Some(CursorKeyword::ZoomIn),
        "zoom-out" => Some(CursorKeyword::ZoomOut),
        _ => None,
    }
}

fn cursor_hotspot_value(value: &PropertyValue) -> Option<f32> {
    match value {
        PropertyValue::Number(n) => Some(*n),
        PropertyValue::Length(l) => Some(l.to_px()),
        _ => None,
    }
}

fn parse_cursor(value: &PropertyValue) -> Option<(Vec<CursorImage>, CursorKeyword)> {
    let tokens: Vec<PropertyValue> = match value {
        PropertyValue::Multiple(list) => list.clone(),
        other => vec![other.clone()],
    };

    if tokens.is_empty() {
        return None;
    }

    let mut images = Vec::new();
    let mut fallback: Option<CursorKeyword> = None;
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            PropertyValue::Keyword(k) if k == "," => {
                idx += 1;
                continue;
            }
            PropertyValue::Url(url) => {
                let mut hotspot = None;
                if idx + 2 < tokens.len() {
                    if let (Some(x), Some(y)) = (
                        cursor_hotspot_value(&tokens[idx + 1]),
                        cursor_hotspot_value(&tokens[idx + 2]),
                    ) {
                        hotspot = Some((x, y));
                        idx += 2;
                    }
                }
                images.push(CursorImage {
                    url: url.clone(),
                    hotspot,
                });
            }
            PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => {
                if let Some(BackgroundImage::Url(url)) = parse_image_set(kw) {
                    images.push(CursorImage { url, hotspot: None });
                }
            }
            PropertyValue::Keyword(kw) => {
                if let Some(parsed) = parse_cursor_keyword(kw) {
                    fallback = Some(parsed);
                    break;
                }
            }
            _ => {}
        }
        idx += 1;
    }

    if images.is_empty() && fallback.is_none() {
        return None;
    }

    Some((images, fallback.unwrap_or(CursorKeyword::Auto)))
}

fn content_value_from_property(value: &PropertyValue) -> Option<ContentValue> {
    let css_text = match value {
        PropertyValue::String(s) => format!("\"{}\"", s),
        PropertyValue::Url(url) => format!("url({})", url),
        PropertyValue::Keyword(kw) => kw.clone(),
        PropertyValue::Multiple(list) => {
            let mut parts = Vec::new();
            for item in list {
                match item {
                    PropertyValue::String(s) => parts.push(format!("\"{}\"", s)),
                    PropertyValue::Url(url) => parts.push(format!("url({})", url)),
                    PropertyValue::Keyword(kw) => parts.push(kw.clone()),
                    PropertyValue::Number(n) => parts.push(n.to_string()),
                    PropertyValue::Percentage(p) => parts.push(format!("{}%", p)),
                    _ => return None,
                }
            }
            parts.join(" ")
        }
        _ => return None,
    };

    parse_content(&css_text)
}

fn parse_background_image_list(value: &PropertyValue) -> Option<Vec<Option<BackgroundImage>>> {
    match value {
        PropertyValue::Multiple(tokens)
            if tokens
                .iter()
                .any(|t| matches!(t, PropertyValue::Keyword(k) if k == ",")) =>
        {
            let mut images = Vec::new();
            for layer_tokens in split_layers(tokens) {
                let candidate = if layer_tokens.len() == 1 {
                    parse_background_image_value(&layer_tokens[0])
                } else {
                    parse_background_image_value(&PropertyValue::Multiple(layer_tokens.clone()))
                };
                match candidate {
                    Some(BackgroundImage::None) => images.push(None),
                    Some(img) => images.push(Some(img)),
                    None => return None,
                }
            }
            if images.is_empty() {
                None
            } else {
                Some(images)
            }
        }
        _ => parse_background_image_value(value).map(|img| match img {
            BackgroundImage::None => vec![None],
            other => vec![Some(other)],
        }),
    }
}

fn parse_layer_list<T>(value: &PropertyValue, parse: impl Fn(&PropertyValue) -> Option<T>) -> Option<Vec<T>> {
    match value {
        PropertyValue::Multiple(tokens)
            if tokens
                .iter()
                .any(|t| matches!(t, PropertyValue::Keyword(k) if k == ",")) =>
        {
            let mut items = Vec::new();
            for layer_tokens in split_layers(tokens) {
                let parsed = if layer_tokens.len() == 1 {
                    parse(&layer_tokens[0])
                } else {
                    parse(&PropertyValue::Multiple(layer_tokens.clone()))
                };
                if let Some(item) = parsed {
                    items.push(item);
                } else {
                    return None;
                }
            }
            if items.is_empty() {
                None
            } else {
                Some(items)
            }
        }
        _ => parse(value).map(|v| vec![v]),
    }
}

fn extract_color_values_with(
    value: &PropertyValue,
    resolver: &dyn Fn(&PropertyValue) -> Option<Rgba>,
) -> Option<Vec<Rgba>> {
    match value {
        PropertyValue::Multiple(values) => {
            let colors: Vec<Rgba> = values.iter().filter_map(resolver).collect();
            if colors.is_empty() {
                None
            } else {
                Some(colors)
            }
        }
        _ => resolver(value).map(|c| vec![c]),
    }
}

fn extract_color_pair_with(
    value: &PropertyValue,
    resolver: &dyn Fn(&PropertyValue) -> Option<Rgba>,
) -> Option<(Rgba, Rgba)> {
    extract_color_values_with(value, resolver).and_then(|colors| {
        if colors.is_empty() {
            None
        } else {
            let first = colors[0];
            let second = *colors.get(1).unwrap_or(&first);
            Some((first, second))
        }
    })
}

fn side_order(orders: &crate::style::SideOrders, side: crate::style::PhysicalSide) -> i32 {
    match side {
        crate::style::PhysicalSide::Top => orders.top,
        crate::style::PhysicalSide::Right => orders.right,
        crate::style::PhysicalSide::Bottom => orders.bottom,
        crate::style::PhysicalSide::Left => orders.left,
    }
}

fn side_order_mut<'a>(orders: &'a mut crate::style::SideOrders, side: crate::style::PhysicalSide) -> &'a mut i32 {
    match side {
        crate::style::PhysicalSide::Top => &mut orders.top,
        crate::style::PhysicalSide::Right => &mut orders.right,
        crate::style::PhysicalSide::Bottom => &mut orders.bottom,
        crate::style::PhysicalSide::Left => &mut orders.left,
    }
}

fn set_margin_side(styles: &mut ComputedStyle, side: crate::style::PhysicalSide, value: Option<Length>, order: i32) {
    if order < side_order(&styles.logical.margin_orders, side) {
        return;
    }
    match side {
        crate::style::PhysicalSide::Top => styles.margin_top = value,
        crate::style::PhysicalSide::Right => styles.margin_right = value,
        crate::style::PhysicalSide::Bottom => styles.margin_bottom = value,
        crate::style::PhysicalSide::Left => styles.margin_left = value,
    }
    *side_order_mut(&mut styles.logical.margin_orders, side) = order;
}

fn set_padding_side(styles: &mut ComputedStyle, side: crate::style::PhysicalSide, value: Length, order: i32) {
    if order < side_order(&styles.logical.padding_orders, side) {
        return;
    }
    match side {
        crate::style::PhysicalSide::Top => styles.padding_top = value,
        crate::style::PhysicalSide::Right => styles.padding_right = value,
        crate::style::PhysicalSide::Bottom => styles.padding_bottom = value,
        crate::style::PhysicalSide::Left => styles.padding_left = value,
    }
    *side_order_mut(&mut styles.logical.padding_orders, side) = order;
}

fn set_border_width_side(styles: &mut ComputedStyle, side: crate::style::PhysicalSide, value: Length, order: i32) {
    if order < side_order(&styles.logical.border_width_orders, side) {
        return;
    }
    match side {
        crate::style::PhysicalSide::Top => styles.border_top_width = value,
        crate::style::PhysicalSide::Right => styles.border_right_width = value,
        crate::style::PhysicalSide::Bottom => styles.border_bottom_width = value,
        crate::style::PhysicalSide::Left => styles.border_left_width = value,
    }
    *side_order_mut(&mut styles.logical.border_width_orders, side) = order;
}

fn set_border_style_side(styles: &mut ComputedStyle, side: crate::style::PhysicalSide, value: BorderStyle, order: i32) {
    if order < side_order(&styles.logical.border_style_orders, side) {
        return;
    }
    match side {
        crate::style::PhysicalSide::Top => styles.border_top_style = value,
        crate::style::PhysicalSide::Right => styles.border_right_style = value,
        crate::style::PhysicalSide::Bottom => styles.border_bottom_style = value,
        crate::style::PhysicalSide::Left => styles.border_left_style = value,
    }
    *side_order_mut(&mut styles.logical.border_style_orders, side) = order;
}

fn set_border_color_side(styles: &mut ComputedStyle, side: crate::style::PhysicalSide, value: Rgba, order: i32) {
    if order < side_order(&styles.logical.border_color_orders, side) {
        return;
    }
    match side {
        crate::style::PhysicalSide::Top => styles.border_top_color = value,
        crate::style::PhysicalSide::Right => styles.border_right_color = value,
        crate::style::PhysicalSide::Bottom => styles.border_bottom_color = value,
        crate::style::PhysicalSide::Left => styles.border_left_color = value,
    }
    *side_order_mut(&mut styles.logical.border_color_orders, side) = order;
}

fn push_logical(styles: &mut ComputedStyle, property: crate::style::LogicalProperty, order: i32) {
    styles
        .logical
        .pending
        .push(crate::style::PendingLogical { order, property });
}

fn set_inset_side(styles: &mut ComputedStyle, side: crate::style::PhysicalSide, value: Option<Length>, order: i32) {
    if order < side_order(&styles.logical.inset_orders, side) {
        return;
    }
    match side {
        crate::style::PhysicalSide::Top => styles.top = value,
        crate::style::PhysicalSide::Right => styles.right = value,
        crate::style::PhysicalSide::Bottom => styles.bottom = value,
        crate::style::PhysicalSide::Left => styles.left = value,
    }
    *side_order_mut(&mut styles.logical.inset_orders, side) = order;
}

fn set_length_with_order(target: &mut Option<Length>, order_slot: &mut i32, value: Option<Length>, order: i32) {
    if order < *order_slot {
        return;
    }
    *target = value;
    *order_slot = order;
}

#[derive(Debug, Clone, Copy)]
enum PhysicalCorner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

fn set_corner_radius(styles: &mut ComputedStyle, corner: PhysicalCorner, value: Option<Length>, order: i32) {
    let order_slot = match corner {
        PhysicalCorner::TopLeft => &mut styles.logical.corner_orders.top_left,
        PhysicalCorner::TopRight => &mut styles.logical.corner_orders.top_right,
        PhysicalCorner::BottomRight => &mut styles.logical.corner_orders.bottom_right,
        PhysicalCorner::BottomLeft => &mut styles.logical.corner_orders.bottom_left,
    };
    if order < *order_slot {
        return;
    }
    match corner {
        PhysicalCorner::TopLeft => {
            if let Some(v) = value {
                styles.border_top_left_radius = v;
            }
        }
        PhysicalCorner::TopRight => {
            if let Some(v) = value {
                styles.border_top_right_radius = v;
            }
        }
        PhysicalCorner::BottomRight => {
            if let Some(v) = value {
                styles.border_bottom_right_radius = v;
            }
        }
        PhysicalCorner::BottomLeft => {
            if let Some(v) = value {
                styles.border_bottom_left_radius = v;
            }
        }
    }
    *order_slot = order;
}

fn set_axis_dimension(styles: &mut ComputedStyle, axis: crate::style::LogicalAxis, value: Option<Length>, order: i32) {
    let is_horizontal = match axis {
        crate::style::LogicalAxis::Inline => inline_axis_is_horizontal(styles.writing_mode),
        crate::style::LogicalAxis::Block => block_axis_is_horizontal(styles.writing_mode),
    };
    if is_horizontal {
        set_length_with_order(&mut styles.width, &mut styles.logical.width_order, value, order);
    } else {
        set_length_with_order(&mut styles.height, &mut styles.logical.height_order, value, order);
    }
}

fn set_axis_min_dimension(
    styles: &mut ComputedStyle,
    axis: crate::style::LogicalAxis,
    value: Option<Length>,
    order: i32,
) {
    let is_horizontal = match axis {
        crate::style::LogicalAxis::Inline => inline_axis_is_horizontal(styles.writing_mode),
        crate::style::LogicalAxis::Block => block_axis_is_horizontal(styles.writing_mode),
    };
    if is_horizontal {
        set_length_with_order(&mut styles.min_width, &mut styles.logical.min_width_order, value, order);
    } else {
        set_length_with_order(
            &mut styles.min_height,
            &mut styles.logical.min_height_order,
            value,
            order,
        );
    }
}

fn set_axis_max_dimension(
    styles: &mut ComputedStyle,
    axis: crate::style::LogicalAxis,
    value: Option<Length>,
    order: i32,
) {
    let is_horizontal = match axis {
        crate::style::LogicalAxis::Inline => inline_axis_is_horizontal(styles.writing_mode),
        crate::style::LogicalAxis::Block => block_axis_is_horizontal(styles.writing_mode),
    };
    if is_horizontal {
        set_length_with_order(&mut styles.max_width, &mut styles.logical.max_width_order, value, order);
    } else {
        set_length_with_order(
            &mut styles.max_height,
            &mut styles.logical.max_height_order,
            value,
            order,
        );
    }
}

pub fn apply_declaration(styles: &mut ComputedStyle, decl: &Declaration, parent_font_size: f32, root_font_size: f32) {
    // Handle CSS Custom Properties (--*)
    if decl.property.starts_with("--") {
        // Preserve the raw custom property value verbatim.
        styles
            .custom_properties
            .insert(decl.property.clone(), decl.raw_value.clone());
        return;
    }

    // Resolve var() references in the value
    let resolved_value = match resolve_var_for_property(&decl.value, &styles.custom_properties, &decl.property) {
        VarResolutionResult::Resolved(v) => v,
        // Unresolved or invalid at computed-value time -> declaration is ignored per spec.
        _ => return,
    };
    let order = styles.logical.next_order();

    let resolve_color_value = |value: &PropertyValue| -> Option<Rgba> {
        match value {
            PropertyValue::Color(c) => Some(*c),
            PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => Some(styles.color),
            _ => None,
        }
    };

    match decl.property.as_str() {
        // Display
        "display" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(display) = Display::parse(kw) {
                    styles.display = display;
                }
            }
        }
        "visibility" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.visibility = match kw.as_str() {
                    "visible" => crate::style::computed::Visibility::Visible,
                    "hidden" => crate::style::computed::Visibility::Hidden,
                    "collapse" => crate::style::computed::Visibility::Collapse,
                    _ => styles.visibility,
                };
            }
        }
        "float" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(value) = Float::parse(kw) {
                    styles.float = value;
                }
            }
        }
        "clear" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(value) = Clear::parse(kw) {
                    styles.clear = value;
                }
            }
        }
        "overflow" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let overflow = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    "clip" => Overflow::Clip,
                    _ => styles.overflow_x,
                };
                styles.overflow_x = overflow;
                styles.overflow_y = overflow;
            }
        }
        "overflow-x" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.overflow_x = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    "clip" => Overflow::Clip,
                    _ => styles.overflow_x,
                };
            }
        }
        "overflow-y" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.overflow_y = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    "clip" => Overflow::Clip,
                    _ => styles.overflow_y,
                };
            }
        }

        // Position
        "position" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(position) = Position::parse(kw) {
                    styles.position = position;
                }
            }
        }
        "box-sizing" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                match kw.as_str() {
                    _ if kw.eq_ignore_ascii_case("content-box") => styles.box_sizing = BoxSizing::ContentBox,
                    _ if kw.eq_ignore_ascii_case("border-box") => styles.box_sizing = BoxSizing::BorderBox,
                    _ => {}
                }
            }
        }

        "top" => set_inset_side(
            styles,
            crate::style::PhysicalSide::Top,
            extract_length(&resolved_value),
            order,
        ),
        "right" => set_inset_side(
            styles,
            crate::style::PhysicalSide::Right,
            extract_length(&resolved_value),
            order,
        ),
        "bottom" => set_inset_side(
            styles,
            crate::style::PhysicalSide::Bottom,
            extract_length(&resolved_value),
            order,
        ),
        "left" => set_inset_side(
            styles,
            crate::style::PhysicalSide::Left,
            extract_length(&resolved_value),
            order,
        ),
        "inset" => {
            if let Some(lengths) = extract_margin_values(&resolved_value) {
                let mut top = styles.top;
                let mut right = styles.right;
                let mut bottom = styles.bottom;
                let mut left = styles.left;
                apply_margin_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
                set_inset_side(styles, crate::style::PhysicalSide::Top, top, order);
                set_inset_side(styles, crate::style::PhysicalSide::Right, right, order);
                set_inset_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
                set_inset_side(styles, crate::style::PhysicalSide::Left, left, order);
            }
        }
        "inset-inline-start" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Inset {
                    axis: crate::style::LogicalAxis::Inline,
                    start: Some(extract_length(&resolved_value)),
                    end: None,
                },
                order,
            );
        }
        "inset-inline-end" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Inset {
                    axis: crate::style::LogicalAxis::Inline,
                    start: None,
                    end: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "inset-block-start" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Inset {
                    axis: crate::style::LogicalAxis::Block,
                    start: Some(extract_length(&resolved_value)),
                    end: None,
                },
                order,
            );
        }
        "inset-block-end" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Inset {
                    axis: crate::style::LogicalAxis::Block,
                    start: None,
                    end: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "inset-inline" => {
            if let Some(values) = extract_margin_values(&resolved_value) {
                let start = values.get(0).cloned().unwrap_or(None);
                let end = values.get(1).cloned().unwrap_or(start);
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Inset {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "inset-block" => {
            if let Some(values) = extract_margin_values(&resolved_value) {
                let start = values.get(0).cloned().unwrap_or(None);
                let end = values.get(1).cloned().unwrap_or(start);
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Inset {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "z-index" => match resolved_value {
            PropertyValue::Number(n) => styles.z_index = Some(n as i32),
            PropertyValue::Keyword(ref kw) if kw.eq_ignore_ascii_case("auto") => styles.z_index = None,
            _ => {}
        },
        "outline-color" => match &resolved_value {
            PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => {
                styles.outline_color = OutlineColor::CurrentColor;
            }
            PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("invert") => {
                styles.outline_color = OutlineColor::Invert;
            }
            PropertyValue::Color(c) => {
                styles.outline_color = OutlineColor::Color(*c);
            }
            _ => {}
        },
        "outline-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(style) = parse_outline_style(kw) {
                    styles.outline_style = style;
                }
            }
        }
        "outline-width" => {
            if let Some(width) = parse_outline_width(&resolved_value) {
                styles.outline_width = width;
            }
        }
        "outline-offset" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.outline_offset = len;
            }
        }
        "outline" => {
            apply_outline_shorthand(styles, &resolved_value);
        }

        // Width and height
        "width" => set_length_with_order(
            &mut styles.width,
            &mut styles.logical.width_order,
            extract_length(&resolved_value),
            order,
        ),
        "height" => set_length_with_order(
            &mut styles.height,
            &mut styles.logical.height_order,
            extract_length(&resolved_value),
            order,
        ),
        "min-width" => set_length_with_order(
            &mut styles.min_width,
            &mut styles.logical.min_width_order,
            extract_length(&resolved_value),
            order,
        ),
        "min-height" => set_length_with_order(
            &mut styles.min_height,
            &mut styles.logical.min_height_order,
            extract_length(&resolved_value),
            order,
        ),
        "max-width" => set_length_with_order(
            &mut styles.max_width,
            &mut styles.logical.max_width_order,
            extract_length(&resolved_value),
            order,
        ),
        "max-height" => set_length_with_order(
            &mut styles.max_height,
            &mut styles.logical.max_height_order,
            extract_length(&resolved_value),
            order,
        ),
        "inline-size" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::InlineSize {
                    value: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "block-size" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::BlockSize {
                    value: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "min-inline-size" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::MinInlineSize {
                    value: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "min-block-size" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::MinBlockSize {
                    value: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "max-inline-size" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::MaxInlineSize {
                    value: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "max-block-size" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::MaxBlockSize {
                    value: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }

        // Margin
        "margin" => {
            if let Some(lengths) = extract_margin_values(&resolved_value) {
                let mut top = styles.margin_top;
                let mut right = styles.margin_right;
                let mut bottom = styles.margin_bottom;
                let mut left = styles.margin_left;
                apply_margin_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
                set_margin_side(styles, crate::style::PhysicalSide::Top, top, order);
                set_margin_side(styles, crate::style::PhysicalSide::Right, right, order);
                set_margin_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
                set_margin_side(styles, crate::style::PhysicalSide::Left, left, order);
            }
        }
        "margin-top" => {
            set_margin_side(
                styles,
                crate::style::PhysicalSide::Top,
                extract_length(&resolved_value),
                order,
            );
        }
        "margin-right" => {
            set_margin_side(
                styles,
                crate::style::PhysicalSide::Right,
                extract_length(&resolved_value),
                order,
            );
        }
        "margin-bottom" => {
            set_margin_side(
                styles,
                crate::style::PhysicalSide::Bottom,
                extract_length(&resolved_value),
                order,
            );
        }
        "margin-left" => {
            set_margin_side(
                styles,
                crate::style::PhysicalSide::Left,
                extract_length(&resolved_value),
                order,
            );
        }
        "margin-inline-start" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Margin {
                    axis: crate::style::LogicalAxis::Inline,
                    start: Some(extract_length(&resolved_value)),
                    end: None,
                },
                order,
            );
        }
        "margin-inline-end" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Margin {
                    axis: crate::style::LogicalAxis::Inline,
                    start: None,
                    end: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "margin-block-start" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Margin {
                    axis: crate::style::LogicalAxis::Block,
                    start: Some(extract_length(&resolved_value)),
                    end: None,
                },
                order,
            );
        }
        "margin-block-end" => {
            push_logical(
                styles,
                crate::style::LogicalProperty::Margin {
                    axis: crate::style::LogicalAxis::Block,
                    start: None,
                    end: Some(extract_length(&resolved_value)),
                },
                order,
            );
        }
        "margin-inline" => {
            if let Some(values) = extract_margin_values(&resolved_value) {
                let start = values.get(0).cloned().unwrap_or(None);
                let end = values.get(1).cloned().unwrap_or(start);
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Margin {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "margin-block" => {
            if let Some(values) = extract_margin_values(&resolved_value) {
                let start = values.get(0).cloned().unwrap_or(None);
                let end = values.get(1).cloned().unwrap_or(start);
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Margin {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }

        // Padding
        "padding" => {
            if let Some(lengths) = extract_box_values(&resolved_value) {
                let mut top = styles.padding_top;
                let mut right = styles.padding_right;
                let mut bottom = styles.padding_bottom;
                let mut left = styles.padding_left;
                apply_box_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
                set_padding_side(styles, crate::style::PhysicalSide::Top, top, order);
                set_padding_side(styles, crate::style::PhysicalSide::Right, right, order);
                set_padding_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
                set_padding_side(styles, crate::style::PhysicalSide::Left, left, order);
            }
        }
        "padding-top" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_padding_side(styles, crate::style::PhysicalSide::Top, len, order);
            }
        }
        "padding-right" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_padding_side(styles, crate::style::PhysicalSide::Right, len, order);
            }
        }
        "padding-bottom" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_padding_side(styles, crate::style::PhysicalSide::Bottom, len, order);
            }
        }
        "padding-left" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_padding_side(styles, crate::style::PhysicalSide::Left, len, order);
            }
        }
        "padding-inline-start" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Padding {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(len),
                        end: None,
                    },
                    order,
                );
            }
        }
        "padding-inline-end" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Padding {
                        axis: crate::style::LogicalAxis::Inline,
                        start: None,
                        end: Some(len),
                    },
                    order,
                );
            }
        }
        "padding-block-start" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Padding {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(len),
                        end: None,
                    },
                    order,
                );
            }
        }
        "padding-block-end" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Padding {
                        axis: crate::style::LogicalAxis::Block,
                        start: None,
                        end: Some(len),
                    },
                    order,
                );
            }
        }
        "padding-inline" => {
            if let Some((start, end)) = extract_length_pair(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Padding {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "padding-block" => {
            if let Some((start, end)) = extract_length_pair(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::Padding {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }

        // Border width
        "border-width" => {
            if let Some(lengths) = extract_box_values(&resolved_value) {
                let mut top = styles.border_top_width;
                let mut right = styles.border_right_width;
                let mut bottom = styles.border_bottom_width;
                let mut left = styles.border_left_width;
                apply_box_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
                set_border_width_side(styles, crate::style::PhysicalSide::Top, top, order);
                set_border_width_side(styles, crate::style::PhysicalSide::Right, right, order);
                set_border_width_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
                set_border_width_side(styles, crate::style::PhysicalSide::Left, left, order);
            }
        }
        "border-top-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_border_width_side(styles, crate::style::PhysicalSide::Top, len, order);
            }
        }
        "border-right-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_border_width_side(styles, crate::style::PhysicalSide::Right, len, order);
            }
        }
        "border-bottom-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_border_width_side(styles, crate::style::PhysicalSide::Bottom, len, order);
            }
        }
        "border-left-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_border_width_side(styles, crate::style::PhysicalSide::Left, len, order);
            }
        }
        "border-inline-start-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderWidth {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(len),
                        end: None,
                    },
                    order,
                );
            }
        }
        "border-inline-end-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderWidth {
                        axis: crate::style::LogicalAxis::Inline,
                        start: None,
                        end: Some(len),
                    },
                    order,
                );
            }
        }
        "border-block-start-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderWidth {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(len),
                        end: None,
                    },
                    order,
                );
            }
        }
        "border-block-end-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderWidth {
                        axis: crate::style::LogicalAxis::Block,
                        start: None,
                        end: Some(len),
                    },
                    order,
                );
            }
        }
        "border-inline-width" => {
            if let Some((start, end)) = extract_length_pair(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderWidth {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "border-block-width" => {
            if let Some((start, end)) = extract_length_pair(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderWidth {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }

        // Border color
        "border-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                set_border_color_side(styles, crate::style::PhysicalSide::Top, c, order);
                set_border_color_side(styles, crate::style::PhysicalSide::Right, c, order);
                set_border_color_side(styles, crate::style::PhysicalSide::Bottom, c, order);
                set_border_color_side(styles, crate::style::PhysicalSide::Left, c, order);
            }
        }
        "border-top-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                set_border_color_side(styles, crate::style::PhysicalSide::Top, c, order);
            }
        }
        "border-right-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                set_border_color_side(styles, crate::style::PhysicalSide::Right, c, order);
            }
        }
        "border-bottom-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                set_border_color_side(styles, crate::style::PhysicalSide::Bottom, c, order);
            }
        }
        "border-left-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                set_border_color_side(styles, crate::style::PhysicalSide::Left, c, order);
            }
        }
        "border-inline-start-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderColor {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(c),
                        end: None,
                    },
                    order,
                );
            }
        }
        "border-inline-end-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderColor {
                        axis: crate::style::LogicalAxis::Inline,
                        start: None,
                        end: Some(c),
                    },
                    order,
                );
            }
        }
        "border-block-start-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderColor {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(c),
                        end: None,
                    },
                    order,
                );
            }
        }
        "border-block-end-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderColor {
                        axis: crate::style::LogicalAxis::Block,
                        start: None,
                        end: Some(c),
                    },
                    order,
                );
            }
        }
        "border-inline-color" => {
            if let Some((start, end)) = extract_color_pair_with(&resolved_value, &resolve_color_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderColor {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "border-block-color" => {
            if let Some((start, end)) = extract_color_pair_with(&resolved_value, &resolve_color_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderColor {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }

        // Border style
        "border-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let style = parse_border_style(kw);
                set_border_style_side(styles, crate::style::PhysicalSide::Top, style, order);
                set_border_style_side(styles, crate::style::PhysicalSide::Right, style, order);
                set_border_style_side(styles, crate::style::PhysicalSide::Bottom, style, order);
                set_border_style_side(styles, crate::style::PhysicalSide::Left, style, order);
            }
        }
        "border-top-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                set_border_style_side(styles, crate::style::PhysicalSide::Top, parse_border_style(kw), order);
            }
        }
        "border-right-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                set_border_style_side(styles, crate::style::PhysicalSide::Right, parse_border_style(kw), order);
            }
        }
        "border-bottom-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                set_border_style_side(
                    styles,
                    crate::style::PhysicalSide::Bottom,
                    parse_border_style(kw),
                    order,
                );
            }
        }
        "border-left-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                set_border_style_side(styles, crate::style::PhysicalSide::Left, parse_border_style(kw), order);
            }
        }
        "border-inline-start-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderStyle {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(parse_border_style(kw)),
                        end: None,
                    },
                    order,
                );
            }
        }
        "border-inline-end-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderStyle {
                        axis: crate::style::LogicalAxis::Inline,
                        start: None,
                        end: Some(parse_border_style(kw)),
                    },
                    order,
                );
            }
        }
        "border-block-start-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderStyle {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(parse_border_style(kw)),
                        end: None,
                    },
                    order,
                );
            }
        }
        "border-block-end-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderStyle {
                        axis: crate::style::LogicalAxis::Block,
                        start: None,
                        end: Some(parse_border_style(kw)),
                    },
                    order,
                );
            }
        }
        "border-inline-style" => {
            let styles_list: Vec<BorderStyle> = match &resolved_value {
                PropertyValue::Keyword(kw) => vec![parse_border_style(kw)],
                PropertyValue::Multiple(values) => values
                    .iter()
                    .filter_map(|v| {
                        if let PropertyValue::Keyword(kw) = v {
                            Some(parse_border_style(kw))
                        } else {
                            None
                        }
                    })
                    .collect(),
                _ => Vec::new(),
            };
            if !styles_list.is_empty() {
                let start = styles_list[0];
                let end = *styles_list.get(1).unwrap_or(&start);
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderStyle {
                        axis: crate::style::LogicalAxis::Inline,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }
        "border-block-style" => {
            let styles_list: Vec<BorderStyle> = match &resolved_value {
                PropertyValue::Keyword(kw) => vec![parse_border_style(kw)],
                PropertyValue::Multiple(values) => values
                    .iter()
                    .filter_map(|v| {
                        if let PropertyValue::Keyword(kw) = v {
                            Some(parse_border_style(kw))
                        } else {
                            None
                        }
                    })
                    .collect(),
                _ => Vec::new(),
            };
            if !styles_list.is_empty() {
                let start = styles_list[0];
                let end = *styles_list.get(1).unwrap_or(&start);
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderStyle {
                        axis: crate::style::LogicalAxis::Block,
                        start: Some(start),
                        end: Some(end),
                    },
                    order,
                );
            }
        }

        // Border (shorthand)
        "border" => {
            if let PropertyValue::Multiple(values) = &resolved_value {
                for val in values {
                    match val {
                        PropertyValue::Length(len) => {
                            set_border_width_side(styles, crate::style::PhysicalSide::Top, *len, order);
                            set_border_width_side(styles, crate::style::PhysicalSide::Right, *len, order);
                            set_border_width_side(styles, crate::style::PhysicalSide::Bottom, *len, order);
                            set_border_width_side(styles, crate::style::PhysicalSide::Left, *len, order);
                        }
                        PropertyValue::Keyword(kw) => {
                            let style = parse_border_style(kw);
                            set_border_style_side(styles, crate::style::PhysicalSide::Top, style, order);
                            set_border_style_side(styles, crate::style::PhysicalSide::Right, style, order);
                            set_border_style_side(styles, crate::style::PhysicalSide::Bottom, style, order);
                            set_border_style_side(styles, crate::style::PhysicalSide::Left, style, order);
                        }
                        PropertyValue::Color(c) => {
                            set_border_color_side(styles, crate::style::PhysicalSide::Top, *c, order);
                            set_border_color_side(styles, crate::style::PhysicalSide::Right, *c, order);
                            set_border_color_side(styles, crate::style::PhysicalSide::Bottom, *c, order);
                            set_border_color_side(styles, crate::style::PhysicalSide::Left, *c, order);
                        }
                        _ => {}
                    }
                }
            }
        }
        "border-inline" => {
            if let PropertyValue::Multiple(values) = &resolved_value {
                let mut width: Option<Length> = None;
                let mut style_val: Option<BorderStyle> = None;
                let mut color: Option<Rgba> = None;
                for val in values {
                    match val {
                        PropertyValue::Length(len) => width = Some(*len),
                        PropertyValue::Keyword(kw) => style_val = Some(parse_border_style(kw)),
                        _ => {
                            if let Some(c) = resolve_color_value(val) {
                                color = Some(c);
                            }
                        }
                    }
                }
                if let Some(w) = width {
                    push_logical(
                        styles,
                        crate::style::LogicalProperty::BorderWidth {
                            axis: crate::style::LogicalAxis::Inline,
                            start: Some(w),
                            end: Some(w),
                        },
                        order,
                    );
                }
                if let Some(st) = style_val {
                    push_logical(
                        styles,
                        crate::style::LogicalProperty::BorderStyle {
                            axis: crate::style::LogicalAxis::Inline,
                            start: Some(st),
                            end: Some(st),
                        },
                        order,
                    );
                }
                if let Some(c) = color {
                    push_logical(
                        styles,
                        crate::style::LogicalProperty::BorderColor {
                            axis: crate::style::LogicalAxis::Inline,
                            start: Some(c),
                            end: Some(c),
                        },
                        order,
                    );
                }
            }
        }
        "border-block" => {
            if let PropertyValue::Multiple(values) = &resolved_value {
                let mut width: Option<Length> = None;
                let mut style_val: Option<BorderStyle> = None;
                let mut color: Option<Rgba> = None;
                for val in values {
                    match val {
                        PropertyValue::Length(len) => width = Some(*len),
                        PropertyValue::Keyword(kw) => style_val = Some(parse_border_style(kw)),
                        _ => {
                            if let Some(c) = resolve_color_value(val) {
                                color = Some(c);
                            }
                        }
                    }
                }
                if let Some(w) = width {
                    push_logical(
                        styles,
                        crate::style::LogicalProperty::BorderWidth {
                            axis: crate::style::LogicalAxis::Block,
                            start: Some(w),
                            end: Some(w),
                        },
                        order,
                    );
                }
                if let Some(st) = style_val {
                    push_logical(
                        styles,
                        crate::style::LogicalProperty::BorderStyle {
                            axis: crate::style::LogicalAxis::Block,
                            start: Some(st),
                            end: Some(st),
                        },
                        order,
                    );
                }
                if let Some(c) = color {
                    push_logical(
                        styles,
                        crate::style::LogicalProperty::BorderColor {
                            axis: crate::style::LogicalAxis::Block,
                            start: Some(c),
                            end: Some(c),
                        },
                        order,
                    );
                }
            }
        }

        // Border radius
        "border-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_corner_radius(styles, PhysicalCorner::TopLeft, Some(len), order);
                set_corner_radius(styles, PhysicalCorner::TopRight, Some(len), order);
                set_corner_radius(styles, PhysicalCorner::BottomLeft, Some(len), order);
                set_corner_radius(styles, PhysicalCorner::BottomRight, Some(len), order);
            }
        }
        "border-top-left-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_corner_radius(styles, PhysicalCorner::TopLeft, Some(len), order);
            }
        }
        "border-top-right-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_corner_radius(styles, PhysicalCorner::TopRight, Some(len), order);
            }
        }
        "border-bottom-left-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_corner_radius(styles, PhysicalCorner::BottomLeft, Some(len), order);
            }
        }
        "border-bottom-right-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                set_corner_radius(styles, PhysicalCorner::BottomRight, Some(len), order);
            }
        }
        "border-start-start-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderCorner {
                        block_start: true,
                        inline_start: true,
                        value: Some(len),
                    },
                    order,
                );
            }
        }
        "border-start-end-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderCorner {
                        block_start: true,
                        inline_start: false,
                        value: Some(len),
                    },
                    order,
                );
            }
        }
        "border-end-start-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderCorner {
                        block_start: false,
                        inline_start: true,
                        value: Some(len),
                    },
                    order,
                );
            }
        }
        "border-end-end-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                push_logical(
                    styles,
                    crate::style::LogicalProperty::BorderCorner {
                        block_start: false,
                        inline_start: false,
                        value: Some(len),
                    },
                    order,
                );
            }
        }

        // Flexbox
        "flex-direction" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.flex_direction = match kw.as_str() {
                    "row" => FlexDirection::Row,
                    "row-reverse" => FlexDirection::RowReverse,
                    "column" => FlexDirection::Column,
                    "column-reverse" => FlexDirection::ColumnReverse,
                    _ => styles.flex_direction,
                };
            }
        }
        "flex-wrap" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.flex_wrap = match kw.as_str() {
                    "nowrap" => FlexWrap::NoWrap,
                    "wrap" => FlexWrap::Wrap,
                    "wrap-reverse" => FlexWrap::WrapReverse,
                    _ => styles.flex_wrap,
                };
            }
        }
        "justify-content" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.justify_content = match kw.as_str() {
                    "flex-start" | "start" => JustifyContent::FlexStart,
                    "flex-end" | "end" => JustifyContent::FlexEnd,
                    "center" => JustifyContent::Center,
                    "space-between" => JustifyContent::SpaceBetween,
                    "space-around" => JustifyContent::SpaceAround,
                    "space-evenly" => JustifyContent::SpaceEvenly,
                    _ => styles.justify_content,
                };
            }
        }
        "align-items" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(value) = parse_align_keyword(kw) {
                    styles.align_items = value;
                }
            }
        }
        "align-self" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.align_self = match kw.as_str() {
                    "auto" => None,
                    _ => parse_align_keyword(kw),
                }
                .or(styles.align_self);
            }
        }
        "align-content" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.align_content = match kw.as_str() {
                    "flex-start" | "start" => AlignContent::FlexStart,
                    "flex-end" | "end" => AlignContent::FlexEnd,
                    "center" => AlignContent::Center,
                    "space-between" => AlignContent::SpaceBetween,
                    "space-evenly" => AlignContent::SpaceEvenly,
                    "space-around" => AlignContent::SpaceAround,
                    "stretch" => AlignContent::Stretch,
                    _ => styles.align_content,
                };
            }
        }
        "justify-items" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                match kw.as_str() {
                    "auto" => styles.justify_items = AlignItems::Stretch,
                    _ => {
                        if let Some(value) = parse_align_keyword(kw) {
                            styles.justify_items = value;
                        }
                    }
                }
            }
        }
        "justify-self" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.justify_self = match kw.as_str() {
                    "auto" => None,
                    _ => parse_align_keyword(kw),
                }
                .or(styles.justify_self);
            }
        }
        "place-items" => {
            if let Some((align, justify)) = parse_place_pair(&resolved_value) {
                styles.align_items = align;
                styles.justify_items = justify;
            }
        }
        "place-self" => {
            if let Some((align, justify)) = parse_place_pair(&resolved_value) {
                styles.align_self = Some(align);
                styles.justify_self = Some(justify);
            }
        }
        "place-content" => {
            if let Some((align, justify)) = parse_place_content_pair(&resolved_value) {
                styles.align_content = align;
                styles.justify_content = justify;
            }
        }
        "flex-grow" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.flex_grow = n;
            }
        }
        "flex-shrink" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.flex_shrink = n;
            }
        }
        "flex-basis" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if kw == "auto" {
                    styles.flex_basis = FlexBasis::Auto;
                }
            } else if let Some(len) = extract_length(&resolved_value) {
                styles.flex_basis = FlexBasis::Length(len);
            }
        }

        // Grid
        "grid-template-columns" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let (tracks, named_lines, line_names) = parse_grid_tracks_with_names(kw);
                styles.grid_template_columns = tracks;
                styles.grid_column_names = named_lines;
                styles.grid_column_line_names = line_names;
            }
        }
        "grid-template-rows" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let (tracks, named_lines, line_names) = parse_grid_tracks_with_names(kw);
                styles.grid_template_rows = tracks;
                styles.grid_row_names = named_lines;
                styles.grid_row_line_names = line_names;
            }
        }
        "grid-template-areas" => match &resolved_value {
            PropertyValue::Keyword(kw) | PropertyValue::String(kw) => {
                if let Some(areas) = parse_grid_template_areas(kw) {
                    let row_count = areas.len();
                    let col_count = areas.first().map(|r| r.len()).unwrap_or(0);
                    if col_count == 0 {
                        return;
                    }

                    // CSS requires area matrices to align with explicit track counts if specified.
                    if (!styles.grid_template_columns.is_empty() && styles.grid_template_columns.len() != col_count)
                        || (!styles.grid_template_rows.is_empty() && styles.grid_template_rows.len() != row_count)
                    {
                        return;
                    }

                    styles.grid_template_areas = areas;

                    if styles.grid_template_columns.is_empty() {
                        styles.grid_template_columns = vec![GridTrack::Auto; col_count];
                        styles.grid_column_line_names = vec![Vec::new(); col_count + 1];
                    }
                    if styles.grid_template_rows.is_empty() {
                        styles.grid_template_rows = vec![GridTrack::Auto; row_count];
                        styles.grid_row_line_names = vec![Vec::new(); row_count + 1];
                    }
                }
            }
            _ => {}
        },
        "grid-template" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(parsed) = parse_grid_template_shorthand(kw) {
                    if let Some((rows, row_line_names)) = parsed.row_tracks {
                        styles.grid_template_rows = rows;
                        styles.grid_row_line_names = row_line_names;
                    }
                    if let Some((cols, col_line_names)) = parsed.column_tracks {
                        styles.grid_template_columns = cols;
                        styles.grid_column_line_names = col_line_names;
                    }
                    if let Some(areas) = parsed.areas {
                        styles.grid_template_areas = areas;
                    }
                }
            }
        }
        "grid" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(parsed) = parse_grid_shorthand(kw) {
                    if let Some(template) = parsed.template {
                        styles.grid_template_areas = template.areas.unwrap_or_default();
                        if let Some((rows, row_names)) = template.row_tracks {
                            styles.grid_template_rows = rows;
                            styles.grid_row_line_names = row_names;
                        }
                        if let Some((cols, col_names)) = template.column_tracks {
                            styles.grid_template_columns = cols;
                            styles.grid_column_line_names = col_names;
                        }
                    }
                    if let Some(rows) = parsed.auto_rows {
                        styles.grid_auto_rows = rows;
                    }
                    if let Some(cols) = parsed.auto_columns {
                        styles.grid_auto_columns = cols;
                    }
                    if let Some(flow) = parsed.auto_flow {
                        styles.grid_auto_flow = flow;
                    }
                }
            }
        }
        "grid-auto-rows" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let ParsedTracks { tracks, .. } = parse_track_list(kw);
                if !tracks.is_empty() {
                    styles.grid_auto_rows = tracks;
                }
            }
        }
        "grid-auto-columns" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let ParsedTracks { tracks, .. } = parse_track_list(kw);
                if !tracks.is_empty() {
                    styles.grid_auto_columns = tracks;
                }
            }
        }
        "grid-auto-flow" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let lower = kw.to_ascii_lowercase();
                let dense = lower.contains("dense");
                let primary = if lower.contains("column") {
                    "column"
                } else if lower.contains("row") {
                    "row"
                } else {
                    "row"
                };
                styles.grid_auto_flow = match (primary, dense) {
                    ("row", false) => GridAutoFlow::Row,
                    ("row", true) => GridAutoFlow::RowDense,
                    ("column", false) => GridAutoFlow::Column,
                    ("column", true) => GridAutoFlow::ColumnDense,
                    _ => GridAutoFlow::Row,
                };
            }
        }
        "grid-gap" | "gap" => {
            if let Some((row, column)) = parse_gap_lengths(&resolved_value) {
                styles.grid_gap = row;
                styles.grid_row_gap = row;
                styles.grid_column_gap = column;
            }
        }
        "grid-row-gap" | "row-gap" => {
            if let Some(len) = parse_single_gap_length(&resolved_value) {
                styles.grid_row_gap = len;
            }
        }
        "grid-column-gap" | "column-gap" => {
            if let Some(len) = parse_single_gap_length(&resolved_value) {
                styles.grid_column_gap = len;
            }
        }
        "grid-column" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                // Store raw value for later resolution (after grid-template-columns is set)
                styles.grid_column_raw = Some(kw.clone());
            }
        }
        "grid-row" => {
            match &resolved_value {
                PropertyValue::Keyword(kw) => {
                    // Store raw value for later resolution (after grid-template-rows is set)
                    styles.grid_row_raw = Some(kw.clone());
                }
                PropertyValue::Number(n) => {
                    // Handle numeric values like "2"
                    styles.grid_row_raw = Some(n.to_string());
                }
                _ => {}
            }
        }
        "grid-column-start" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                // For explicit start/end, we can parse immediately if numeric
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_column_start = n;
                } else {
                    // Store in grid_column_raw for deferred resolution
                    let current_end = styles
                        .grid_column_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(_, e)| e.trim()))
                        .unwrap_or("auto");
                    styles.grid_column_raw = Some(format!("{} / {}", kw, current_end));
                }
            }
        }
        "grid-column-end" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_column_end = n;
                } else {
                    let current_start = styles
                        .grid_column_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(s, _)| s.trim()))
                        .unwrap_or("auto");
                    styles.grid_column_raw = Some(format!("{} / {}", current_start, kw));
                }
            }
        }
        "grid-row-start" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_row_start = n;
                } else {
                    let current_end = styles
                        .grid_row_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(_, e)| e.trim()))
                        .unwrap_or("auto");
                    styles.grid_row_raw = Some(format!("{} / {}", kw, current_end));
                }
            }
        }
        "grid-row-end" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(n) = kw.parse::<i32>() {
                    styles.grid_row_end = n;
                } else {
                    let current_start = styles
                        .grid_row_raw
                        .as_ref()
                        .and_then(|s| s.split_once('/').map(|(s, _)| s.trim()))
                        .unwrap_or("auto");
                    styles.grid_row_raw = Some(format!("{} / {}", current_start, kw));
                }
            }
        }
        "grid-area" => {
            if let PropertyValue::Keyword(kw) | PropertyValue::String(kw) = &resolved_value {
                let parts: Vec<&str> = kw.split('/').map(|s| s.trim()).filter(|s| !s.is_empty()).collect();
                if parts.is_empty() {
                    return;
                }
                let mut row_start = parts.get(0).copied().unwrap_or("auto").to_string();
                let mut col_start = parts.get(1).copied().unwrap_or("auto").to_string();
                let mut row_end = parts.get(2).copied().unwrap_or("auto").to_string();
                let mut col_end = parts.get(3).copied().unwrap_or("auto").to_string();
                if parts.len() == 1 {
                    // Single area name: map to area start/end
                    row_start = format!("{}-start", parts[0]);
                    row_end = format!("{}-end", parts[0]);
                    col_start = row_start.clone();
                    col_end = row_end.clone();
                }
                styles.grid_row_raw = Some(format!("{} / {}", row_start, row_end));
                styles.grid_column_raw = Some(format!("{} / {}", col_start, col_end));
            }
        }

        // Typography
        "font" => {
            if let PropertyValue::Keyword(raw) = &resolved_value {
                if let Some((font_style, font_weight, font_variant, font_stretch, font_size, line_height, families)) =
                    parse_font_shorthand(raw, parent_font_size, root_font_size)
                {
                    styles.font_variant_ligatures = FontVariantLigatures::default();
                    styles.font_variant_caps = FontVariantCaps::Normal;
                    styles.font_variant_numeric = FontVariantNumeric::default();
                    styles.font_variant_east_asian = FontVariantEastAsian::default();
                    styles.font_variant_position = FontVariantPosition::Normal;
                    styles.font_size_adjust = FontSizeAdjust::None;
                    styles.font_synthesis = FontSynthesis::default();
                    styles.font_kerning = FontKerning::Auto;
                    styles.font_feature_settings.clear();
                    styles.font_style = font_style;
                    styles.font_weight = font_weight;
                    styles.font_variant = font_variant;
                    styles.font_stretch = font_stretch;
                    styles.font_size = font_size;
                    styles.line_height = line_height;
                    styles.font_family = families;
                }
            }
        }
        "font-family" => {
            if let PropertyValue::FontFamily(families) = &resolved_value {
                styles.font_family = families.clone();
            }
        }
        "font-size" => {
            match &resolved_value {
                PropertyValue::Keyword(kw) => {
                    if let Some(size) = parse_font_size_keyword(kw, parent_font_size) {
                        styles.font_size = size;
                    }
                }
                PropertyValue::Length(len) => {
                    // Resolve font-size against parent or root depending on unit
                    if len.unit.is_absolute() {
                        styles.font_size = len.to_px();
                    } else if len.unit == LengthUnit::Em {
                        styles.font_size = len.value * parent_font_size;
                    } else if len.unit == LengthUnit::Rem {
                        styles.font_size = len.value * root_font_size;
                    } else if len.unit == LengthUnit::Percent {
                        styles.font_size = (len.value / 100.0) * parent_font_size;
                    }
                }
                PropertyValue::Percentage(p) => {
                    styles.font_size = (p / 100.0) * parent_font_size;
                }
                _ => {}
            }
        }
        "font-size-adjust" => match &resolved_value {
            PropertyValue::Keyword(kw) => match kw.as_str() {
                "none" => styles.font_size_adjust = FontSizeAdjust::None,
                "from-font" => styles.font_size_adjust = FontSizeAdjust::FromFont,
                _ => {}
            },
            PropertyValue::Number(n) if *n >= 0.0 => {
                styles.font_size_adjust = FontSizeAdjust::Number(*n);
            }
            _ => {}
        },
        "font-weight" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                styles.font_weight = match kw.as_str() {
                    "normal" => FontWeight::Normal,
                    "bold" => FontWeight::Bold,
                    "lighter" => FontWeight::Lighter,
                    "bolder" => FontWeight::Bolder,
                    _ => styles.font_weight,
                };
            }
            PropertyValue::Number(n) => {
                if (1.0..=1000.0).contains(n) {
                    styles.font_weight = FontWeight::Number((*n as u16).clamp(1, 1000));
                }
            }
            _ => {}
        },
        "font-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(fs) = parse_font_style_keyword(kw) {
                    styles.font_style = fs;
                }
            }
        }
        "font-variant" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                // Accept both the legacy shorthand and caps values.
                let tokens: Vec<&str> = kw.split_whitespace().collect();
                if tokens.len() == 1 && tokens[0] == "normal" {
                    styles.font_variant = FontVariant::Normal;
                    styles.font_variant_caps = FontVariantCaps::Normal;
                    styles.font_variant_alternates = FontVariantAlternates::default();
                } else {
                    for tok in tokens {
                        match tok {
                            "small-caps" => {
                                styles.font_variant = FontVariant::SmallCaps;
                                styles.font_variant_caps = FontVariantCaps::SmallCaps;
                            }
                            "all-small-caps" => {
                                styles.font_variant_caps = FontVariantCaps::AllSmallCaps;
                            }
                            "petite-caps" => {
                                styles.font_variant_caps = FontVariantCaps::PetiteCaps;
                            }
                            "all-petite-caps" => {
                                styles.font_variant_caps = FontVariantCaps::AllPetiteCaps;
                            }
                            "unicase" => {
                                styles.font_variant_caps = FontVariantCaps::Unicase;
                            }
                            "titling-caps" => {
                                styles.font_variant_caps = FontVariantCaps::TitlingCaps;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        "font-variant-caps" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.font_variant_caps = match kw.as_str() {
                    "normal" => FontVariantCaps::Normal,
                    "small-caps" => {
                        styles.font_variant = FontVariant::SmallCaps;
                        FontVariantCaps::SmallCaps
                    }
                    "all-small-caps" => FontVariantCaps::AllSmallCaps,
                    "petite-caps" => FontVariantCaps::PetiteCaps,
                    "all-petite-caps" => FontVariantCaps::AllPetiteCaps,
                    "unicase" => FontVariantCaps::Unicase,
                    "titling-caps" => FontVariantCaps::TitlingCaps,
                    _ => styles.font_variant_caps,
                };
            }
        }
        "font-variant-alternates" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let trimmed = kw.trim();
                if trimmed == "normal" {
                    styles.font_variant_alternates = FontVariantAlternates::default();
                } else {
                    let mut alt = FontVariantAlternates::default();
                    for token in trimmed.split_whitespace() {
                        if token == "historical-forms" {
                            alt.historical_forms = true;
                            continue;
                        }
                        if let Some(inner) = token.strip_prefix("stylistic(").and_then(|s| s.strip_suffix(')')) {
                            if let Ok(n) = inner.trim().parse::<u8>() {
                                alt.stylistic = Some(n);
                            }
                            continue;
                        }
                        if let Some(inner) = token.strip_prefix("styleset(").and_then(|s| s.strip_suffix(')')) {
                            for part in inner
                                .split(|c: char| c == ',' || c.is_whitespace())
                                .filter(|s| !s.is_empty())
                            {
                                if let Ok(n) = part.parse::<u8>() {
                                    alt.stylesets.push(n);
                                }
                            }
                            continue;
                        }
                        if let Some(inner) = token
                            .strip_prefix("character-variant(")
                            .and_then(|s| s.strip_suffix(')'))
                        {
                            for part in inner
                                .split(|c: char| c == ',' || c.is_whitespace())
                                .filter(|s| !s.is_empty())
                            {
                                if let Ok(n) = part.parse::<u8>() {
                                    alt.character_variants.push(n);
                                }
                            }
                            continue;
                        }
                        if let Some(inner) = token.strip_prefix("swash(").and_then(|s| s.strip_suffix(')')) {
                            if let Ok(n) = inner.trim().parse::<u8>() {
                                alt.swash = Some(n);
                            }
                            continue;
                        }
                        if let Some(inner) = token.strip_prefix("ornaments(").and_then(|s| s.strip_suffix(')')) {
                            if let Ok(n) = inner.trim().parse::<u8>() {
                                alt.ornaments = Some(n);
                            }
                            continue;
                        }
                        if let Some(inner) = token.strip_prefix("annotation(").and_then(|s| s.strip_suffix(')')) {
                            if !inner.trim().is_empty() {
                                alt.annotation = Some(inner.trim().to_string());
                            }
                            continue;
                        }
                    }
                    styles.font_variant_alternates = alt;
                }
            }
        }
        "font-variant-position" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.font_variant_position = match kw.as_str() {
                    "normal" => FontVariantPosition::Normal,
                    "sub" => FontVariantPosition::Sub,
                    "super" => FontVariantPosition::Super,
                    _ => styles.font_variant_position,
                };
            }
        }
        "font-variant-east-asian" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let tokens: Vec<&str> = kw.split_whitespace().collect();
                if tokens.len() == 1 && tokens[0] == "normal" {
                    styles.font_variant_east_asian = FontVariantEastAsian::default();
                } else if !tokens.is_empty() {
                    let mut east = FontVariantEastAsian::default();
                    for tok in tokens {
                        match tok {
                            "jis78" => east.variant = Some(EastAsianVariant::Jis78),
                            "jis83" => east.variant = Some(EastAsianVariant::Jis83),
                            "jis90" => east.variant = Some(EastAsianVariant::Jis90),
                            "jis04" => east.variant = Some(EastAsianVariant::Jis04),
                            "simplified" => east.variant = Some(EastAsianVariant::Simplified),
                            "traditional" => east.variant = Some(EastAsianVariant::Traditional),
                            "full-width" => east.width = Some(EastAsianWidth::FullWidth),
                            "proportional-width" => east.width = Some(EastAsianWidth::ProportionalWidth),
                            "ruby" => east.ruby = true,
                            _ => {}
                        }
                    }
                    styles.font_variant_east_asian = east;
                }
            }
        }
        "font-variant-numeric" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let tokens: Vec<&str> = kw.split_whitespace().collect();
                if tokens.len() == 1 && tokens[0] == "normal" {
                    styles.font_variant_numeric = FontVariantNumeric::default();
                } else if !tokens.is_empty() {
                    let mut numeric = FontVariantNumeric::default();
                    for tok in tokens {
                        match tok {
                            "lining-nums" => numeric.figure = NumericFigure::Lining,
                            "oldstyle-nums" => numeric.figure = NumericFigure::Oldstyle,
                            "proportional-nums" => numeric.spacing = NumericSpacing::Proportional,
                            "tabular-nums" => numeric.spacing = NumericSpacing::Tabular,
                            "diagonal-fractions" => numeric.fraction = NumericFraction::Diagonal,
                            "stacked-fractions" => numeric.fraction = NumericFraction::Stacked,
                            "ordinal" => numeric.ordinal = true,
                            "slashed-zero" => numeric.slashed_zero = true,
                            _ => {}
                        }
                    }
                    styles.font_variant_numeric = numeric;
                }
            }
        }
        "font-variant-ligatures" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let tokens: Vec<&str> = kw.split_whitespace().collect();
                if tokens.len() == 1 {
                    match tokens[0] {
                        "normal" => styles.font_variant_ligatures = FontVariantLigatures::default(),
                        "none" => {
                            styles.font_variant_ligatures = FontVariantLigatures {
                                common: false,
                                discretionary: false,
                                historical: false,
                                contextual: false,
                            }
                        }
                        _ => {}
                    }
                } else if !tokens.is_empty() {
                    // Start from the initial value and apply toggles.
                    let mut lig = FontVariantLigatures::default();
                    for tok in tokens {
                        match tok {
                            "common-ligatures" => lig.common = true,
                            "no-common-ligatures" => lig.common = false,
                            "discretionary-ligatures" => lig.discretionary = true,
                            "no-discretionary-ligatures" => lig.discretionary = false,
                            "historical-ligatures" => lig.historical = true,
                            "no-historical-ligatures" => lig.historical = false,
                            "contextual" => lig.contextual = true,
                            "no-contextual" => lig.contextual = false,
                            _ => {}
                        }
                    }
                    styles.font_variant_ligatures = lig;
                }
            }
        }
        "font-feature-settings" => {
            if let PropertyValue::Keyword(raw) = &resolved_value {
                let trimmed = raw.trim();
                if trimmed.eq_ignore_ascii_case("normal") {
                    styles.font_feature_settings.clear();
                } else {
                    let mut input = ParserInput::new(trimmed);
                    let mut parser = Parser::new(&mut input);
                    if let Ok(features) = parser.parse_comma_separated(|p| parse_feature_setting(p)) {
                        styles.font_feature_settings = features;
                    }
                }
            }
        }
        "font-stretch" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                if let Some(stretch) = parse_font_stretch_keyword(kw) {
                    styles.font_stretch = stretch;
                }
            }
            PropertyValue::Percentage(p) => {
                styles.font_stretch = FontStretch::from_percentage(*p);
            }
            _ => {}
        },
        "font-kerning" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.font_kerning = match kw.as_str() {
                    "auto" => FontKerning::Auto,
                    "normal" => FontKerning::Normal,
                    "none" => FontKerning::None,
                    _ => styles.font_kerning,
                };
            }
        }
        "font-synthesis" => {
            if let PropertyValue::Keyword(raw) = &resolved_value {
                let tokens: Vec<&str> = raw.split_whitespace().collect();
                if tokens.len() == 1 && tokens[0] == "none" {
                    styles.font_synthesis = FontSynthesis {
                        weight: false,
                        style: false,
                        small_caps: false,
                    };
                } else if !tokens.is_empty() {
                    let mut synth = FontSynthesis {
                        weight: false,
                        style: false,
                        small_caps: false,
                    };
                    for tok in tokens {
                        match tok {
                            "weight" => synth.weight = true,
                            "style" => synth.style = true,
                            "small-caps" => synth.small_caps = true,
                            _ => {}
                        }
                    }
                    styles.font_synthesis = synth;
                }
            }
        }
        "line-height" => match &resolved_value {
            PropertyValue::Keyword(kw) if kw == "normal" => {
                styles.line_height = LineHeight::Normal;
            }
            PropertyValue::Number(n) => {
                styles.line_height = LineHeight::Number(*n);
            }
            PropertyValue::Length(len) => {
                styles.line_height = LineHeight::Length(*len);
            }
            PropertyValue::Percentage(pct) => {
                styles.line_height = LineHeight::Number(*pct / 100.0);
            }
            _ => {}
        },
        "table-layout" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.table_layout = match kw.as_str() {
                    "auto" => TableLayout::Auto,
                    "fixed" => TableLayout::Fixed,
                    _ => styles.table_layout,
                };
            }
        }
        "empty-cells" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.empty_cells = match kw.as_str() {
                    "show" => EmptyCells::Show,
                    "hide" => EmptyCells::Hide,
                    _ => styles.empty_cells,
                };
            }
        }
        "caption-side" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.caption_side = match kw.as_str() {
                    "top" => CaptionSide::Top,
                    "bottom" => CaptionSide::Bottom,
                    _ => styles.caption_side,
                };
            }
        }
        "vertical-align" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                styles.vertical_align = match kw.as_str() {
                    "baseline" => VerticalAlign::Baseline,
                    "sub" => VerticalAlign::Sub,
                    "super" => VerticalAlign::Super,
                    "text-top" => VerticalAlign::TextTop,
                    "text-bottom" => VerticalAlign::TextBottom,
                    "middle" => VerticalAlign::Middle,
                    "top" => VerticalAlign::Top,
                    "bottom" => VerticalAlign::Bottom,
                    _ => styles.vertical_align,
                };
            }
            PropertyValue::Length(len) => {
                styles.vertical_align = VerticalAlign::Length(*len);
            }
            PropertyValue::Percentage(pct) => {
                styles.vertical_align = VerticalAlign::Percentage(*pct);
            }
            _ => {}
        },
        "text-align" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let parsed = match kw.as_str() {
                    "start" => Some(TextAlign::Start),
                    "end" => Some(TextAlign::End),
                    "left" => Some(TextAlign::Left),
                    "right" => Some(TextAlign::Right),
                    "center" => Some(TextAlign::Center),
                    "justify" => Some(TextAlign::Justify),
                    "justify-all" => Some(TextAlign::Justify),
                    "match-parent" => Some(TextAlign::MatchParent),
                    _ => None,
                };
                if let Some(value) = parsed {
                    styles.text_align = value;
                    styles.text_align_last = match value {
                        TextAlign::Justify => TextAlignLast::Auto,
                        TextAlign::MatchParent => TextAlignLast::MatchParent,
                        _ => TextAlignLast::Auto,
                    };
                    if matches!(value, TextAlign::Justify) && kw.as_str() == "justify-all" {
                        styles.text_align_last = TextAlignLast::Justify;
                    }
                }
            }
        }
        "text-align-all" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let parsed = match kw.as_str() {
                    "start" => Some(TextAlign::Start),
                    "end" => Some(TextAlign::End),
                    "left" => Some(TextAlign::Left),
                    "right" => Some(TextAlign::Right),
                    "center" => Some(TextAlign::Center),
                    "justify" => Some(TextAlign::Justify),
                    "justify-all" => Some(TextAlign::Justify),
                    "match-parent" => Some(TextAlign::MatchParent),
                    _ => None,
                };
                if let Some(value) = parsed {
                    styles.text_align = value;
                    match value {
                        TextAlign::Justify => {
                            styles.text_align_last = TextAlignLast::Justify;
                        }
                        TextAlign::MatchParent => {
                            // text-align-last left unchanged per spec carve-out
                        }
                        _ => {
                            styles.text_align_last = TextAlignLast::Auto;
                        }
                    }
                }
            }
        }
        "text-align-last" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_align_last = match kw.as_str() {
                    "auto" => TextAlignLast::Auto,
                    "start" => TextAlignLast::Start,
                    "end" => TextAlignLast::End,
                    "left" => TextAlignLast::Left,
                    "right" => TextAlignLast::Right,
                    "center" => TextAlignLast::Center,
                    "justify" => TextAlignLast::Justify,
                    "match-parent" => TextAlignLast::MatchParent,
                    _ => styles.text_align_last,
                };
            }
        }
        "text-justify" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_justify = match kw.as_str() {
                    "auto" => TextJustify::Auto,
                    "none" => TextJustify::None,
                    "inter-word" => TextJustify::InterWord,
                    "inter-character" => TextJustify::InterCharacter,
                    "distribute" => TextJustify::Distribute,
                    _ => styles.text_justify,
                };
            }
        }
        "text-orientation" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_orientation = match kw.as_str() {
                    "mixed" => TextOrientation::Mixed,
                    "upright" => TextOrientation::Upright,
                    "sideways" => TextOrientation::Sideways,
                    "sideways-right" => TextOrientation::SidewaysRight,
                    _ => styles.text_orientation,
                };
            }
        }
        "text-combine-upright" => {
            if let Some(value) = parse_text_combine_upright(&resolved_value) {
                styles.text_combine_upright = value;
            }
        }
        "text-indent" => {
            let mut length = styles.text_indent.length;
            let mut hanging = false;
            let mut each_line = false;

            let mut apply_component = |value: &PropertyValue| match value {
                PropertyValue::Length(len) => length = *len,
                PropertyValue::Percentage(pct) => length = Length::percent(*pct),
                PropertyValue::Keyword(kw) => match kw.as_str() {
                    "hanging" => hanging = true,
                    "each-line" => each_line = true,
                    _ => {}
                },
                _ => {}
            };

            match &resolved_value {
                PropertyValue::Multiple(values) => {
                    for v in values {
                        apply_component(v);
                    }
                }
                other => apply_component(other),
            }

            styles.text_indent = TextIndent {
                length,
                hanging,
                each_line,
            };
        }
        "direction" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.direction = match kw.as_str() {
                    "ltr" => Direction::Ltr,
                    "rtl" => Direction::Rtl,
                    _ => styles.direction,
                };
            }
        }
        "text-decoration-line" => {
            if let Some(lines) = parse_text_decoration_line(&resolved_value) {
                styles.text_decoration_line_specified = true;
                styles.text_decoration.lines = lines;
            }
        }
        "text-decoration-style" => {
            if let Some(style) = parse_text_decoration_style(&resolved_value) {
                styles.text_decoration.style = style;
            }
        }
        "text-decoration-color" => {
            if let Some(color) = parse_text_decoration_color(&resolved_value) {
                styles.text_decoration.color = color;
            }
        }
        "text-decoration-thickness" => {
            if let Some(thick) = parse_text_decoration_thickness(&resolved_value, parent_font_size, root_font_size) {
                styles.text_decoration.thickness = thick;
            }
        }
        "text-decoration-skip-ink" => {
            if let Some(skip) = parse_text_decoration_skip_ink(&resolved_value) {
                styles.text_decoration_skip_ink = skip;
            }
        }
        "text-underline-offset" => {
            if let Some(offset) = parse_text_underline_offset(&resolved_value) {
                styles.text_underline_offset = offset;
            }
        }
        "text-underline-position" => {
            if let Some(pos) = parse_text_underline_position(&resolved_value) {
                styles.text_underline_position = pos;
            }
        }
        "text-emphasis-style" => {
            if let Some(emph) = parse_text_emphasis_style(&resolved_value) {
                styles.text_emphasis_style = emph;
            }
        }
        "text-emphasis-color" => {
            if let Some(color) = parse_text_emphasis_color(&resolved_value) {
                styles.text_emphasis_color = color;
            }
        }
        "text-emphasis-position" => {
            if let Some(pos) = parse_text_emphasis_position(&resolved_value) {
                styles.text_emphasis_position = pos;
            }
        }
        "text-emphasis" => {
            if let Some((style_val, color_val)) = parse_text_emphasis_shorthand(&resolved_value) {
                if let Some(emph_style) = style_val {
                    styles.text_emphasis_style = emph_style;
                } else {
                    styles.text_emphasis_style = TextEmphasisStyle::None;
                }
                if let Some(color) = color_val {
                    styles.text_emphasis_color = color;
                } else {
                    styles.text_emphasis_color = None;
                }
            }
        }
        "text-decoration" => {
            let tokens: Vec<PropertyValue> = match resolved_value {
                PropertyValue::Multiple(ref values) => values.clone(),
                _ => vec![resolved_value.clone()],
            };

            if tokens.is_empty() {
                return;
            }

            // Reset to initial values per shorthand rules.
            let mut decoration = TextDecoration::default();
            styles.text_decoration_line_specified = true;
            for token in tokens {
                if let Some(lines) = parse_text_decoration_line(&token) {
                    decoration.lines = lines;
                    continue;
                }
                if let Some(style) = parse_text_decoration_style(&token) {
                    decoration.style = style;
                    continue;
                }
                if let Some(color) = parse_text_decoration_color(&token) {
                    decoration.color = color;
                    continue;
                }
                if let Some(thick) = parse_text_decoration_thickness(&token, parent_font_size, root_font_size) {
                    decoration.thickness = thick;
                }
            }

            styles.text_decoration = decoration;
        }
        "list-style-type" => {
            if let Some(t) = parse_list_style_type(&resolved_value) {
                styles.list_style_type = t;
            }
        }
        "list-style-position" => {
            if let Some(p) = parse_list_style_position(&resolved_value) {
                styles.list_style_position = p;
            }
        }
        "list-style-image" => {
            if let Some(img) = parse_list_style_image(&resolved_value) {
                styles.list_style_image = img;
            }
        }
        "list-style" => {
            let tokens: Vec<PropertyValue> = match resolved_value {
                PropertyValue::Multiple(ref values) => values.clone(),
                _ => vec![resolved_value.clone()],
            };

            if tokens.is_empty() {
                return;
            }

            // Reset to initial values
            let mut list_type = ListStyleType::Disc;
            let mut list_pos = ListStylePosition::Outside;
            let mut list_image = ListStyleImage::None;

            for token in tokens {
                if let Some(t) = parse_list_style_type(&token) {
                    list_type = t;
                    continue;
                }
                if let Some(p) = parse_list_style_position(&token) {
                    list_pos = p;
                    continue;
                }
                if let Some(img) = parse_list_style_image(&token) {
                    list_image = img;
                }
            }

            styles.list_style_type = list_type;
            styles.list_style_position = list_pos;
            styles.list_style_image = list_image;
        }
        "counter-reset" => {
            if let Some(parsed) = parse_counter_property(&resolved_value, CounterPropertyKind::Reset) {
                styles.counters.counter_reset = Some(parsed);
            }
        }
        "counter-increment" => {
            if let Some(parsed) = parse_counter_property(&resolved_value, CounterPropertyKind::Increment) {
                styles.counters.counter_increment = Some(parsed);
            }
        }
        "counter-set" => {
            if let Some(parsed) = parse_counter_property(&resolved_value, CounterPropertyKind::Set) {
                styles.counters.counter_set = Some(parsed);
            }
        }
        "text-transform" => {
            if let Some(parsed) = parse_text_transform(&resolved_value) {
                styles.text_transform = parsed;
            }
        }
        "letter-spacing" => {
            let font_size = if styles.font_size.is_finite() && styles.font_size > 0.0 {
                styles.font_size
            } else {
                parent_font_size
            };
            if let Some(len) = parse_spacing_value(&resolved_value, font_size, root_font_size, false) {
                styles.letter_spacing = len;
            }
        }
        "word-spacing" => {
            let font_size = if styles.font_size.is_finite() && styles.font_size > 0.0 {
                styles.font_size
            } else {
                parent_font_size
            };
            if let Some(len) = parse_spacing_value(&resolved_value, font_size, root_font_size, true) {
                styles.word_spacing = len;
            }
        }
        "white-space" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.white_space = match kw.as_str() {
                    "normal" => WhiteSpace::Normal,
                    "nowrap" => WhiteSpace::Nowrap,
                    "pre" => WhiteSpace::Pre,
                    "pre-wrap" => WhiteSpace::PreWrap,
                    "pre-line" => WhiteSpace::PreLine,
                    "break-spaces" => WhiteSpace::BreakSpaces,
                    _ => styles.white_space,
                };
            }
        }
        "line-break" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.line_break = match kw.as_str() {
                    "auto" => LineBreak::Auto,
                    "loose" => LineBreak::Loose,
                    "normal" => LineBreak::Normal,
                    "strict" => LineBreak::Strict,
                    "anywhere" => LineBreak::Anywhere,
                    _ => styles.line_break,
                };
            }
        }
        "tab-size" => match &resolved_value {
            PropertyValue::Number(n) => {
                styles.tab_size = TabSize::Number(n.max(0.0));
            }
            PropertyValue::Length(len) if !len.value.is_nan() => {
                styles.tab_size = TabSize::Length(*len);
            }
            _ => {}
        },
        "hyphens" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.hyphens = match kw.as_str() {
                    "none" => HyphensMode::None,
                    "manual" => HyphensMode::Manual,
                    "auto" => HyphensMode::Auto,
                    _ => styles.hyphens,
                };
            }
        }
        "word-break" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.word_break = match kw.as_str() {
                    "normal" => WordBreak::Normal,
                    "break-all" => WordBreak::BreakAll,
                    "keep-all" => WordBreak::KeepAll,
                    "break-word" => WordBreak::BreakWord,
                    _ => styles.word_break,
                };
            }
        }
        "overflow-wrap" | "word-wrap" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.overflow_wrap = match kw.as_str() {
                    "normal" => OverflowWrap::Normal,
                    "break-word" => OverflowWrap::BreakWord,
                    "anywhere" => OverflowWrap::Anywhere,
                    _ => styles.overflow_wrap,
                };
            }
        }
        "unicode-bidi" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.unicode_bidi = match kw.as_str() {
                    "normal" => UnicodeBidi::Normal,
                    "embed" => UnicodeBidi::Embed,
                    "bidi-override" => UnicodeBidi::BidiOverride,
                    "isolate" => UnicodeBidi::Isolate,
                    "isolate-override" => UnicodeBidi::IsolateOverride,
                    "plaintext" => UnicodeBidi::Plaintext,
                    _ => styles.unicode_bidi,
                };
            }
        }
        "writing-mode" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.writing_mode = match kw.as_str() {
                    "horizontal-tb" => WritingMode::HorizontalTb,
                    "vertical-rl" => WritingMode::VerticalRl,
                    "vertical-lr" => WritingMode::VerticalLr,
                    "sideways-rl" => WritingMode::SidewaysRl,
                    "sideways-lr" => WritingMode::SidewaysLr,
                    _ => styles.writing_mode,
                };
            }
        }
        "cursor" => {
            if let Some((images, keyword)) = parse_cursor(&resolved_value) {
                styles.cursor_images = images;
                styles.cursor = keyword;
            }
        }

        // Color
        "color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                styles.color = c;
            }
        }
        "background-color" => {
            if let Some(c) = resolve_color_value(&resolved_value) {
                styles.background_color = c;
            }
        }

        // Background
        "background-image" => {
            if let Some(images) = parse_background_image_list(&resolved_value) {
                styles.background_images = images;
                styles.rebuild_background_layers();
            }
        }
        "background-size" => {
            if let Some(sizes) = parse_layer_list(&resolved_value, parse_background_size) {
                styles.background_sizes = sizes;
                styles.rebuild_background_layers();
            }
        }
        "background-repeat" => {
            if let Some(repeats) = parse_layer_list(&resolved_value, parse_background_repeat) {
                styles.background_repeats = repeats;
                styles.rebuild_background_layers();
            }
        }
        "background-position" => {
            if let Some(positions) = parse_layer_list(&resolved_value, parse_background_position) {
                styles.background_positions = positions;
                styles.rebuild_background_layers();
            }
        }
        "background-position-x" => {
            if let Some(xs) = parse_layer_list(&resolved_value, parse_background_position_component_x) {
                styles.ensure_background_lists();
                let default = match BackgroundLayer::default().position {
                    BackgroundPosition::Position { x, .. } => x,
                };
                let layer_count = xs.len().max(styles.background_positions.len()).max(1);
                let mut positions = Vec::with_capacity(layer_count);
                for idx in 0..layer_count {
                    let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
                    let source = styles
                        .background_positions
                        .get(source_idx)
                        .cloned()
                        .unwrap_or(BackgroundLayer::default().position.clone());
                    let x_comp = xs
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| xs.last().copied().unwrap_or(default));
                    let BackgroundPosition::Position { y, .. } = source;
                    positions.push(BackgroundPosition::Position { x: x_comp, y });
                }
                styles.background_positions = positions;
                styles.rebuild_background_layers();
            }
        }
        "background-position-y" => {
            if let Some(ys) = parse_layer_list(&resolved_value, parse_background_position_component_y) {
                styles.ensure_background_lists();
                let default = match BackgroundLayer::default().position {
                    BackgroundPosition::Position { y, .. } => y,
                };
                let layer_count = ys.len().max(styles.background_positions.len()).max(1);
                let mut positions = Vec::with_capacity(layer_count);
                for idx in 0..layer_count {
                    let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
                    let source = styles
                        .background_positions
                        .get(source_idx)
                        .cloned()
                        .unwrap_or(BackgroundLayer::default().position.clone());
                    let y_comp = ys
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| ys.last().copied().unwrap_or(default));
                    let BackgroundPosition::Position { x, .. } = source;
                    positions.push(BackgroundPosition::Position { x, y: y_comp });
                }
                styles.background_positions = positions;
                styles.rebuild_background_layers();
            }
        }
        "background-attachment" => {
            let parse = |value: &PropertyValue| match value {
                PropertyValue::Keyword(kw) => match kw.as_str() {
                    "scroll" => Some(BackgroundAttachment::Scroll),
                    "fixed" => Some(BackgroundAttachment::Fixed),
                    "local" => Some(BackgroundAttachment::Local),
                    _ => None,
                },
                _ => None,
            };
            if let Some(attachments) = parse_layer_list(&resolved_value, parse) {
                styles.background_attachments = attachments;
                styles.rebuild_background_layers();
            }
        }
        "background-origin" => {
            if let Some(origins) = parse_layer_list(&resolved_value, parse_background_box) {
                styles.background_origins = origins;
                styles.rebuild_background_layers();
            }
        }
        "background-clip" => {
            if let Some(clips) = parse_layer_list(&resolved_value, parse_background_box) {
                styles.background_clips = clips;
                styles.rebuild_background_layers();
            }
        }
        "background-blend-mode" => {
            let parse = |value: &PropertyValue| match value {
                PropertyValue::Keyword(kw) => parse_mix_blend_mode(kw),
                _ => None,
            };
            if let Some(modes) = parse_layer_list(&resolved_value, parse) {
                styles.background_blend_modes = modes;
                styles.rebuild_background_layers();
            }
        }

        // Shorthand: background
        "background" => {
            let tokens: Vec<PropertyValue> = match resolved_value {
                PropertyValue::Multiple(ref parts) => parts.clone(),
                _ => vec![resolved_value.clone()],
            };

            if tokens.is_empty() {
                return;
            }

            let layers = split_layers(&tokens);
            let mut parsed_layers = Vec::new();
            for layer_tokens in layers {
                if let Some(parsed) = parse_background_shorthand(&layer_tokens, styles.color) {
                    parsed_layers.push(parsed);
                }
            }
            if parsed_layers.is_empty() {
                return;
            }

            styles.background_color = parsed_layers.last().and_then(|p| p.color).unwrap_or(Rgba::TRANSPARENT);

            let mut layers = Vec::new();
            for parsed in parsed_layers {
                let mut layer = BackgroundLayer::default();
                layer.image = parsed.image;
                if let Some(rep) = parsed.repeat {
                    layer.repeat = rep;
                }
                if let Some(pos) = parsed.position {
                    layer.position = pos;
                }
                if let Some(size) = parsed.size {
                    layer.size = size;
                }
                if let Some(att) = parsed.attachment {
                    layer.attachment = att;
                }
                if let Some(origin) = parsed.origin {
                    layer.origin = origin;
                }
                if let Some(clip) = parsed.clip {
                    layer.clip = clip;
                }
                layers.push(layer);
            }
            styles.set_background_layers(layers);
            styles.rebuild_background_layers();
        }

        // Visual effects
        "opacity" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.opacity = n.clamp(0.0, 1.0);
            }
        }
        "box-shadow" => match &resolved_value {
            PropertyValue::BoxShadow(shadows) => {
                styles.box_shadow = shadows.clone();
            }
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.box_shadow.clear();
            }
            _ => {}
        },
        "text-shadow" => match &resolved_value {
            PropertyValue::TextShadow(shadows) => {
                styles.text_shadow = shadows.clone();
            }
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.text_shadow.clear();
            }
            _ => {}
        },
        "transform" => {
            if let PropertyValue::Transform(transforms) = &resolved_value {
                styles.transform = transforms.clone();
            }
        }
        "filter" => {
            if let Some(filters) = parse_filter_list(&resolved_value) {
                styles.filter = filters;
            }
        }
        "backdrop-filter" => {
            if let Some(filters) = parse_filter_list(&resolved_value) {
                styles.backdrop_filter = filters;
            }
        }
        "transform-origin" => {
            if let Some(origin) = parse_transform_origin(&resolved_value) {
                styles.transform_origin = origin;
            }
        }
        "mix-blend-mode" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(mode) = parse_mix_blend_mode(kw) {
                    styles.mix_blend_mode = mode;
                }
            }
        }
        "isolation" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.isolation = match kw.as_str() {
                    "isolate" => Isolation::Isolate,
                    _ => Isolation::Auto,
                };
            }
        }
        "will-change" => {
            if let Some(value) = parse_will_change(&resolved_value) {
                styles.will_change = value;
            }
        }
        "contain" => {
            if let Some(value) = parse_containment(&resolved_value) {
                styles.containment = value;
            }
        }

        "border-collapse" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_collapse = match kw.as_str() {
                    "collapse" => crate::style::types::BorderCollapse::Collapse,
                    "separate" => crate::style::types::BorderCollapse::Separate,
                    _ => styles.border_collapse,
                };
            }
        }
        "border-spacing" => {
            if let Some((h, v)) = extract_length_pair(&resolved_value) {
                styles.border_spacing_horizontal = h;
                styles.border_spacing_vertical = v;
            }
        }

        // Content property (for ::before and ::after pseudo-elements)
        "content" => {
            if let Some(parsed) = content_value_from_property(&resolved_value) {
                styles.content_value = parsed.clone();
                // Keep legacy string storage for marker tests and existing code paths.
                styles.content = match &resolved_value {
                    PropertyValue::String(s) => s.clone(),
                    PropertyValue::Keyword(k) => k.clone(),
                    PropertyValue::Multiple(tokens) => tokens
                        .iter()
                        .map(|t| match t {
                            PropertyValue::String(s) => format!("\"{}\"", s),
                            PropertyValue::Keyword(k) => k.clone(),
                            PropertyValue::Url(u) => format!("url({})", u),
                            PropertyValue::Number(n) => n.to_string(),
                            PropertyValue::Percentage(p) => format!("{}%", p),
                            _ => String::new(),
                        })
                        .collect::<Vec<_>>()
                        .join(" "),
                    PropertyValue::Url(u) => format!("url({})", u),
                    _ => String::new(),
                };
            }
        }
        "quotes" => {
            match &resolved_value {
                PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("none") => {
                    styles.quotes.clear();
                }
                PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => {
                    styles.quotes = crate::style::content::default_quotes();
                }
                PropertyValue::Multiple(list) if list.iter().all(|v| matches!(v, PropertyValue::String(_))) => {
                    let strings: Vec<String> = list
                        .iter()
                        .filter_map(|v| {
                            if let PropertyValue::String(s) = v {
                                Some(s.clone())
                            } else {
                                None
                            }
                        })
                        .collect();
                    if strings.len() % 2 == 0 && !strings.is_empty() {
                        styles.quotes = strings
                            .chunks(2)
                            .map(|pair| (pair[0].clone(), pair[1].clone()))
                            .collect();
                    }
                }
                _ => {}
            };
        }
        "image-rendering" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(rendering) = parse_image_rendering(kw) {
                    styles.image_rendering = rendering;
                }
            }
        }
        "aspect-ratio" => {
            if let Some(ratio) = parse_aspect_ratio(&resolved_value) {
                styles.aspect_ratio = ratio;
            }
        }
        "object-fit" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Some(fit) = parse_object_fit(kw) {
                    styles.object_fit = fit;
                }
            }
        }
        "object-position" => {
            if let Some(pos) = parse_object_position(&resolved_value) {
                styles.object_position = pos;
            }
        }

        _ => {
            // Ignore unknown properties
        }
    }
}

pub fn extract_length(value: &PropertyValue) -> Option<Length> {
    match value {
        PropertyValue::Length(len) => Some(*len),
        PropertyValue::Number(n) if *n == 0.0 => Some(Length::px(0.0)),
        PropertyValue::Keyword(kw) if kw == "auto" => None,
        _ => None,
    }
}

pub fn extract_length_pair(value: &PropertyValue) -> Option<(Length, Length)> {
    match value {
        PropertyValue::Length(len) => Some((*len, *len)),
        PropertyValue::Multiple(values) => {
            let lengths: Vec<Length> = values.iter().filter_map(extract_length).collect();
            match lengths.len() {
                1 => Some((lengths[0], lengths[0])),
                l if l >= 2 => Some((lengths[0], lengths[1])),
                _ => None,
            }
        }
        _ => None,
    }
}

fn parse_gap_token(token: &str) -> Option<Length> {
    let t = token.trim();
    if t.is_empty() {
        return None;
    }
    if t.eq_ignore_ascii_case("normal") {
        return Some(Length::px(0.0));
    }
    parse_length(t)
}

fn extract_gap_component(value: &PropertyValue) -> Option<Length> {
    match value {
        PropertyValue::Length(len) => Some(*len),
        PropertyValue::Percentage(p) => Some(Length::percent(*p)),
        PropertyValue::Number(n) if *n == 0.0 => Some(Length::px(0.0)),
        PropertyValue::Keyword(kw) => parse_gap_token(kw),
        _ => None,
    }
}

fn parse_gap_lengths(value: &PropertyValue) -> Option<(Length, Length)> {
    match value {
        PropertyValue::Multiple(values) => {
            let lengths: Vec<Length> = values.iter().filter_map(extract_gap_component).collect();
            match lengths.as_slice() {
                [first] => Some((*first, *first)),
                [first, second, ..] => Some((*first, *second)),
                _ => None,
            }
        }
        PropertyValue::Keyword(kw) => {
            let tokens: Vec<&str> = kw.split_whitespace().filter(|s| !s.is_empty()).collect();
            if tokens.is_empty() {
                return None;
            }
            let mut lengths = Vec::new();
            for token in tokens.iter().take(2) {
                if let Some(len) = parse_gap_token(token) {
                    lengths.push(len);
                }
            }
            match lengths.as_slice() {
                [first] => Some((*first, *first)),
                [first, second] => Some((*first, *second)),
                _ => None,
            }
        }
        PropertyValue::Length(len) => Some((*len, *len)),
        PropertyValue::Percentage(p) => Some((Length::percent(*p), Length::percent(*p))),
        PropertyValue::Number(n) if *n == 0.0 => Some((Length::px(0.0), Length::px(0.0))),
        _ => None,
    }
}

fn parse_single_gap_length(value: &PropertyValue) -> Option<Length> {
    parse_gap_lengths(value).map(|(first, _)| first)
}

fn parse_align_keyword(kw: &str) -> Option<AlignItems> {
    match kw {
        "start" => Some(AlignItems::Start),
        "end" => Some(AlignItems::End),
        "self-start" => Some(AlignItems::SelfStart),
        "self-end" => Some(AlignItems::SelfEnd),
        "flex-start" => Some(AlignItems::FlexStart),
        "flex-end" => Some(AlignItems::FlexEnd),
        "center" => Some(AlignItems::Center),
        "baseline" => Some(AlignItems::Baseline),
        "stretch" => Some(AlignItems::Stretch),
        "normal" => Some(AlignItems::Stretch),
        "left" => Some(AlignItems::Start),
        "right" => Some(AlignItems::End),
        _ => None,
    }
}

fn parse_place_pair(value: &PropertyValue) -> Option<(AlignItems, AlignItems)> {
    let tokens: Vec<String> = match value {
        PropertyValue::Multiple(values) => values
            .iter()
            .filter_map(|v| match v {
                PropertyValue::Keyword(k) => Some(k.clone()),
                _ => None,
            })
            .collect(),
        PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
        _ => Vec::new(),
    };
    if tokens.is_empty() {
        return None;
    }
    if tokens.len() == 1 {
        if let Some(val) = parse_align_keyword(&tokens[0]) {
            return Some((val, val));
        }
        return None;
    }
    let first = parse_align_keyword(&tokens[0])?;
    let second = parse_align_keyword(&tokens[1])?;
    Some((first, second))
}

fn parse_place_content_pair(value: &PropertyValue) -> Option<(AlignContent, JustifyContent)> {
    fn to_align_content(kw: &str) -> Option<AlignContent> {
        match kw {
            "start" | "flex-start" => Some(AlignContent::FlexStart),
            "end" | "flex-end" => Some(AlignContent::FlexEnd),
            "center" => Some(AlignContent::Center),
            "stretch" | "normal" => Some(AlignContent::Stretch),
            "space-between" => Some(AlignContent::SpaceBetween),
            "space-around" => Some(AlignContent::SpaceAround),
            "space-evenly" => Some(AlignContent::SpaceEvenly),
            _ => None,
        }
    }

    fn to_justify_content(kw: &str) -> Option<JustifyContent> {
        match kw {
            "flex-start" | "start" => Some(JustifyContent::FlexStart),
            "flex-end" | "end" => Some(JustifyContent::FlexEnd),
            "center" => Some(JustifyContent::Center),
            "space-between" => Some(JustifyContent::SpaceBetween),
            "space-around" => Some(JustifyContent::SpaceAround),
            "space-evenly" => Some(JustifyContent::SpaceEvenly),
            _ => None,
        }
    }

    let tokens: Vec<String> = match value {
        PropertyValue::Multiple(values) => values
            .iter()
            .filter_map(|v| match v {
                PropertyValue::Keyword(k) => Some(k.clone()),
                _ => None,
            })
            .collect(),
        PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
        _ => Vec::new(),
    };
    if tokens.is_empty() {
        return None;
    }
    if tokens.len() == 1 {
        if let (Some(a), Some(j)) = (to_align_content(&tokens[0]), to_justify_content(&tokens[0])) {
            return Some((a, j));
        }
        return None;
    }
    let first = to_align_content(&tokens[0])?;
    let second = to_justify_content(&tokens[1])?;
    Some((first, second))
}

fn parse_spacing_value(
    value: &PropertyValue,
    font_size: f32,
    root_font_size: f32,
    allow_percentage: bool,
) -> Option<f32> {
    match value {
        PropertyValue::Keyword(kw) if kw == "normal" => Some(0.0),
        PropertyValue::Number(n) if *n == 0.0 => Some(0.0),
        PropertyValue::Length(len) => resolve_font_relative_length(*len, font_size, root_font_size),
        PropertyValue::Percentage(pct) if allow_percentage => Some((pct / 100.0) * font_size),
        _ => None,
    }
}

fn resolve_font_relative_length(len: Length, font_size: f32, root_font_size: f32) -> Option<f32> {
    Some(match len.unit {
        u if u.is_absolute() => len.to_px(),
        LengthUnit::Em => len.value * font_size,
        LengthUnit::Ex => len.value * font_size * 0.5,
        LengthUnit::Ch => len.value * font_size * 0.5,
        LengthUnit::Rem => len.value * root_font_size,
        LengthUnit::Percent => (len.value / 100.0) * font_size,
        // Fallback: keep the raw author value when we cannot resolve viewport-relative or unknown units here.
        _ => len.value,
    })
}

fn parse_object_fit(kw: &str) -> Option<ObjectFit> {
    match kw {
        "fill" => Some(ObjectFit::Fill),
        "contain" => Some(ObjectFit::Contain),
        "cover" => Some(ObjectFit::Cover),
        "none" => Some(ObjectFit::None),
        "scale-down" => Some(ObjectFit::ScaleDown),
        _ => None,
    }
}

fn parse_image_rendering(kw: &str) -> Option<ImageRendering> {
    match kw {
        "auto" => Some(ImageRendering::Auto),
        "smooth" | "high-quality" | "optimizequality" => Some(ImageRendering::Smooth),
        "crisp-edges" | "crispedges" | "optimize-contrast" | "optimizecontrast" => Some(ImageRendering::CrispEdges),
        "pixelated" => Some(ImageRendering::Pixelated),
        "optimizespeed" => Some(ImageRendering::CrispEdges),
        _ => None,
    }
}

fn parse_aspect_ratio(value: &PropertyValue) -> Option<AspectRatio> {
    match value {
        PropertyValue::Keyword(kw) => {
            if kw.eq_ignore_ascii_case("auto") {
                return Some(AspectRatio::Auto);
            }
            parse_ratio_string(kw).map(AspectRatio::Ratio)
        }
        PropertyValue::Number(n) => (*n > 0.0).then_some(AspectRatio::Ratio(*n)),
        PropertyValue::Multiple(values) => {
            let mut nums: Vec<f32> = Vec::new();
            for v in values {
                match v {
                    PropertyValue::Number(n) if *n > 0.0 => nums.push(*n),
                    PropertyValue::Keyword(kw) => {
                        if let Ok(n) = kw.parse::<f32>() {
                            if n > 0.0 {
                                nums.push(n);
                            }
                        }
                    }
                    _ => {}
                }
            }
            match nums.as_slice() {
                [num] => Some(AspectRatio::Ratio(*num)),
                [num, denom, ..] if *denom > 0.0 => Some(AspectRatio::Ratio(*num / *denom)),
                _ => None,
            }
        }
        _ => None,
    }
}

fn parse_ratio_string(raw: &str) -> Option<f32> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if let Ok(val) = trimmed.parse::<f32>() {
        return (val > 0.0).then_some(val);
    }
    if let Some((num_str, denom_str)) = trimmed.split_once('/') {
        let num = num_str.trim().parse::<f32>().ok()?;
        let denom = denom_str.trim().parse::<f32>().ok()?;
        if num > 0.0 && denom > 0.0 {
            return Some(num / denom);
        }
    }
    None
}

fn parse_object_position(value: &PropertyValue) -> Option<ObjectPosition> {
    use crate::style::types::{PositionComponent as PC, PositionKeyword as PK};

    #[derive(Copy, Clone, Eq, PartialEq)]
    enum Axis {
        Horizontal,
        Vertical,
    }

    fn parse_component(value: &PropertyValue) -> Option<(PC, Option<Axis>)> {
        match value {
            PropertyValue::Length(len) => Some((PC::Length(*len), None)),
            PropertyValue::Percentage(pct) => Some((PC::Percentage(*pct / 100.0), None)),
            PropertyValue::Keyword(kw) => match kw.as_str() {
                "left" => Some((PC::Keyword(PK::Start), Some(Axis::Horizontal))),
                "right" => Some((PC::Keyword(PK::End), Some(Axis::Horizontal))),
                "center" => Some((PC::Keyword(PK::Center), None)),
                "top" => Some((PC::Keyword(PK::Start), Some(Axis::Vertical))),
                "bottom" => Some((PC::Keyword(PK::End), Some(Axis::Vertical))),
                _ => None,
            },
            _ => None,
        }
    }

    let values: Vec<&PropertyValue> = match value {
        PropertyValue::Multiple(list) if !list.is_empty() => list.iter().collect(),
        _ => vec![value],
    };

    let mut parsed: Vec<(PC, Option<Axis>)> = values.iter().filter_map(|v| parse_component(v)).collect();
    if parsed.is_empty() {
        return None;
    }

    let default = PC::Keyword(PK::Center);
    if parsed.len() == 1 {
        let (comp, axis) = parsed.remove(0);
        let (x, y) = match axis {
            Some(Axis::Vertical) => (default, comp),
            _ => (comp, default),
        };
        return Some(ObjectPosition { x, y });
    }

    let (first, first_axis) = parsed.get(0).copied().unwrap_or((default, None));
    let (second, second_axis) = parsed.get(1).copied().unwrap_or((default, None));

    let (x, y) = match (first_axis, second_axis) {
        (Some(Axis::Vertical), Some(Axis::Horizontal)) => (second, first),
        (Some(Axis::Vertical), None) => (second, first),
        _ => (first, second),
    };

    Some(ObjectPosition { x, y })
}

fn parse_background_size_component(value: &PropertyValue) -> Option<BackgroundSizeComponent> {
    match value {
        PropertyValue::Keyword(kw) if kw == "auto" => Some(BackgroundSizeComponent::Auto),
        PropertyValue::Length(len) => Some(BackgroundSizeComponent::Length(*len)),
        PropertyValue::Number(n) if *n == 0.0 => Some(BackgroundSizeComponent::Length(Length::px(0.0))),
        PropertyValue::Percentage(p) => Some(BackgroundSizeComponent::Length(Length::percent(*p))),
        _ => None,
    }
}

fn parse_background_size(value: &PropertyValue) -> Option<BackgroundSize> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "cover" => Some(BackgroundSize::Keyword(BackgroundSizeKeyword::Cover)),
            "contain" => Some(BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)),
            "auto" => Some(BackgroundSize::Explicit(
                BackgroundSizeComponent::Auto,
                BackgroundSizeComponent::Auto,
            )),
            _ => None,
        },
        PropertyValue::Multiple(values) => {
            if values.len() == 1 {
                if let Some(single) = parse_background_size(&values[0]) {
                    return Some(single);
                }
            }
            let components: Vec<BackgroundSizeComponent> =
                values.iter().filter_map(parse_background_size_component).collect();
            match components.len() {
                0 => None,
                1 => Some(BackgroundSize::Explicit(components[0], BackgroundSizeComponent::Auto)),
                _ => Some(BackgroundSize::Explicit(components[0], components[1])),
            }
        }
        _ => parse_background_size_component(value).map(|c| BackgroundSize::Explicit(c, BackgroundSizeComponent::Auto)),
    }
}

fn parse_transform_origin(value: &PropertyValue) -> Option<TransformOrigin> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum AxisHint {
        Horizontal,
        Vertical,
        Either,
    }

    fn keyword_to_length(kw: &str) -> Option<(Length, AxisHint)> {
        match kw {
            "left" => Some((Length::percent(0.0), AxisHint::Horizontal)),
            "right" => Some((Length::percent(100.0), AxisHint::Horizontal)),
            "top" => Some((Length::percent(0.0), AxisHint::Vertical)),
            "bottom" => Some((Length::percent(100.0), AxisHint::Vertical)),
            "center" => Some((Length::percent(50.0), AxisHint::Either)),
            _ => None,
        }
    }

    fn push_component(target_x: &mut Option<Length>, target_y: &mut Option<Length>, component: Length, hint: AxisHint) {
        match hint {
            AxisHint::Horizontal => {
                if target_x.is_none() {
                    *target_x = Some(component);
                }
            }
            AxisHint::Vertical => {
                if target_y.is_none() {
                    *target_y = Some(component);
                }
            }
            AxisHint::Either => {
                if target_x.is_none() {
                    *target_x = Some(component);
                } else if target_y.is_none() {
                    *target_y = Some(component);
                }
            }
        }
    }

    let components: Vec<&PropertyValue> = match value {
        PropertyValue::Multiple(values) if !values.is_empty() => values.iter().collect(),
        _ => vec![value],
    };

    let mut x: Option<Length> = None;
    let mut y: Option<Length> = None;

    for comp in components.into_iter().take(3) {
        match comp {
            PropertyValue::Length(len) => push_component(&mut x, &mut y, *len, AxisHint::Either),
            PropertyValue::Percentage(pct) => push_component(&mut x, &mut y, Length::percent(*pct), AxisHint::Either),
            PropertyValue::Keyword(kw) => {
                if let Some((len, hint)) = keyword_to_length(kw) {
                    push_component(&mut x, &mut y, len, hint);
                }
            }
            PropertyValue::Number(n) if *n == 0.0 => push_component(&mut x, &mut y, Length::px(0.0), AxisHint::Either),
            _ => {}
        }
    }

    let x = x.unwrap_or_else(|| Length::percent(50.0));
    let y = y.unwrap_or_else(|| Length::percent(50.0));
    Some(TransformOrigin { x, y })
}

fn parse_will_change(value: &PropertyValue) -> Option<WillChange> {
    let text = will_change_value_as_string(value)?;
    parse_will_change_from_str(&text)
}

fn parse_containment(value: &PropertyValue) -> Option<Containment> {
    let text = match value {
        PropertyValue::Keyword(k) | PropertyValue::String(k) => Some(k.clone()),
        PropertyValue::Multiple(values) if !values.is_empty() => {
            let mut out = String::new();
            for token in values {
                match token {
                    PropertyValue::Keyword(k) => {
                        if !out.is_empty() {
                            out.push(' ');
                        }
                        out.push_str(k);
                    }
                    PropertyValue::String(s) => {
                        if !out.is_empty() {
                            out.push(' ');
                        }
                        out.push_str(s);
                    }
                    _ => return None,
                }
            }
            Some(out)
        }
        _ => None,
    }?;

    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }

    let mut input = ParserInput::new(trimmed);
    let mut parser = Parser::new(&mut input);

    if parser.try_parse(|p| p.expect_ident_matching("none")).is_ok() {
        parser.skip_whitespace();
        return if parser.is_exhausted() {
            Some(Containment::none())
        } else {
            None
        };
    }
    if parser.try_parse(|p| p.expect_ident_matching("strict")).is_ok() {
        parser.skip_whitespace();
        return if parser.is_exhausted() {
            Some(Containment::strict())
        } else {
            None
        };
    }
    if parser.try_parse(|p| p.expect_ident_matching("content")).is_ok() {
        parser.skip_whitespace();
        return if parser.is_exhausted() {
            Some(Containment::content())
        } else {
            None
        };
    }

    let mut size = false;
    let mut inline_size = false;
    let mut layout = false;
    let mut style = false;
    let mut paint = false;
    let mut saw_any = false;

    while !parser.is_exhausted() {
        parser.skip_whitespace();
        let ident = match parser.try_parse(|p| p.expect_ident().map(|i| i.as_ref().to_ascii_lowercase())) {
            Ok(i) => i,
            Err(_) => return None,
        };
        match ident.as_str() {
            "size" => {
                size = true;
                saw_any = true;
            }
            "inline-size" => {
                inline_size = true;
                saw_any = true;
            }
            "layout" => {
                layout = true;
                saw_any = true;
            }
            "style" => {
                style = true;
                saw_any = true;
            }
            "paint" => {
                paint = true;
                saw_any = true;
            }
            _ => return None,
        }
        parser.skip_whitespace();
    }

    if saw_any {
        Some(Containment::with_flags(size, inline_size, layout, style, paint))
    } else {
        None
    }
}

fn will_change_value_as_string(value: &PropertyValue) -> Option<String> {
    match value {
        PropertyValue::Keyword(kw) | PropertyValue::String(kw) => Some(kw.clone()),
        PropertyValue::Multiple(values) => {
            let mut out = String::new();
            for token in values {
                match token {
                    PropertyValue::Keyword(k) => {
                        if !out.is_empty() && k != "," {
                            out.push(' ');
                        }
                        out.push_str(k);
                        if k == "," {
                            out.push(' ');
                        }
                    }
                    PropertyValue::String(s) => {
                        if !out.is_empty() {
                            out.push(' ');
                        }
                        out.push_str(s);
                    }
                    _ => return None,
                }
            }
            let trimmed = out.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        }
        _ => None,
    }
}

fn parse_will_change_from_str(text: &str) -> Option<WillChange> {
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }

    let mut input = ParserInput::new(trimmed);
    let mut parser = Parser::new(&mut input);

    if parser.try_parse(|p| p.expect_ident_matching("auto")).is_ok() {
        parser.skip_whitespace();
        return if parser.is_exhausted() {
            Some(WillChange::Auto)
        } else {
            None
        };
    }

    let mut hints = Vec::new();
    while !parser.is_exhausted() {
        parser.skip_whitespace();
        let ident = parser.expect_ident().ok()?;
        let ident_lower = ident.as_ref().to_ascii_lowercase();
        match ident_lower.as_str() {
            "auto" => return None,
            "scroll-position" => hints.push(WillChangeHint::ScrollPosition),
            "contents" => hints.push(WillChangeHint::Contents),
            other => hints.push(WillChangeHint::Property(other.to_string())),
        }
        parser.skip_whitespace();
        if parser.is_exhausted() {
            break;
        }
        parser.expect_comma().ok()?;
    }

    if hints.is_empty() {
        None
    } else {
        Some(WillChange::Hints(hints))
    }
}

fn parse_filter_list(value: &PropertyValue) -> Option<Vec<FilterFunction>> {
    let text = match value {
        PropertyValue::Keyword(kw) => kw.as_str(),
        PropertyValue::String(s) => s.as_str(),
        PropertyValue::Multiple(values) if values.len() == 1 => match &values[0] {
            PropertyValue::Keyword(kw) => kw.as_str(),
            PropertyValue::String(s) => s.as_str(),
            _ => return None,
        },
        _ => return None,
    };

    let trimmed = text.trim();
    if trimmed.eq_ignore_ascii_case("none") {
        return Some(Vec::new());
    }

    let mut input = ParserInput::new(trimmed);
    let mut parser = Parser::new(&mut input);
    let mut filters = Vec::new();

    while !parser.is_exhausted() {
        parser.skip_whitespace();
        if parser.is_exhausted() {
            break;
        }

        let func_name = match parser.next() {
            Ok(Token::Function(name)) => name.as_ref().to_ascii_lowercase(),
            _ => return None,
        };

        let parsed = parser
            .parse_nested_block(|block| parse_filter_function(&func_name, block))
            .ok()?;
        filters.push(parsed);
        parser.skip_whitespace();
    }

    Some(filters)
}

fn parse_filter_function<'i, 't>(
    name: &str,
    input: &mut Parser<'i, 't>,
) -> Result<FilterFunction, cssparser::ParseError<'i, ()>> {
    match name {
        "blur" => {
            let len = parse_length_component(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Blur(len))
        }
        "brightness" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Brightness(v))
        }
        "contrast" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Contrast(v))
        }
        "grayscale" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Grayscale(v))
        }
        "sepia" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Sepia(v))
        }
        "saturate" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Saturate(v))
        }
        "hue-rotate" => {
            let v = parse_angle_degrees(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::HueRotate(v))
        }
        "invert" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Invert(v))
        }
        "opacity" => {
            let v = parse_number_or_percentage(input)?;
            input.skip_whitespace();
            if !input.is_exhausted() {
                return Err(input.new_custom_error(()));
            }
            Ok(FilterFunction::Opacity(v))
        }
        "drop-shadow" => parse_drop_shadow(input),
        _ => Err(input.new_custom_error(())),
    }
}

fn parse_number_or_percentage<'i, 't>(input: &mut Parser<'i, 't>) -> Result<f32, cssparser::ParseError<'i, ()>> {
    let location = input.current_source_location();
    match input.next()? {
        Token::Number { value, .. } => Ok(*value),
        Token::Percentage { unit_value, .. } => Ok(*unit_value),
        _ => Err(location.new_custom_error(())),
    }
}

fn parse_angle_token(token: &Token) -> Option<f32> {
    match token {
        Token::Dimension { value, ref unit, .. } => match unit.as_ref() {
            "deg" => Some(*value),
            "grad" => Some(*value * 0.9),
            "turn" => Some(*value * 360.0),
            "rad" => Some(*value * (180.0 / std::f32::consts::PI)),
            _ => None,
        },
        Token::Number { value, .. } if *value == 0.0 => Some(0.0),
        _ => None,
    }
}

fn parse_font_style_keyword(raw: &str) -> Option<FontStyle> {
    let lower = raw.trim().to_ascii_lowercase();
    if lower == "normal" {
        return Some(FontStyle::Normal);
    }
    if lower == "italic" {
        return Some(FontStyle::Italic);
    }
    if lower.starts_with("oblique") {
        let angle_part = lower.trim_start_matches("oblique").trim();
        if angle_part.is_empty() {
            return Some(FontStyle::Oblique(None));
        }
        // Attempt to parse angle; fall back to plain oblique if invalid.
        if let Some(angle) = parse_angle_from_str(angle_part) {
            return Some(FontStyle::Oblique(Some(angle)));
        }
        return Some(FontStyle::Oblique(None));
    }
    None
}

fn parse_angle_from_str(s: &str) -> Option<f32> {
    let mut input = ParserInput::new(s.trim());
    let mut parser = Parser::new(&mut input);
    parse_angle_degrees(&mut parser).ok()
}

fn parse_font_stretch_keyword(kw: &str) -> Option<FontStretch> {
    match kw {
        "ultra-condensed" => Some(FontStretch::UltraCondensed),
        "extra-condensed" => Some(FontStretch::ExtraCondensed),
        "condensed" => Some(FontStretch::Condensed),
        "semi-condensed" => Some(FontStretch::SemiCondensed),
        "normal" => Some(FontStretch::Normal),
        "semi-expanded" => Some(FontStretch::SemiExpanded),
        "expanded" => Some(FontStretch::Expanded),
        "extra-expanded" => Some(FontStretch::ExtraExpanded),
        "ultra-expanded" => Some(FontStretch::UltraExpanded),
        _ => None,
    }
}

fn parse_font_shorthand(
    value: &str,
    parent_font_size: f32,
    root_font_size: f32,
) -> Option<(
    FontStyle,
    FontWeight,
    FontVariant,
    FontStretch,
    f32,
    LineHeight,
    Vec<String>,
)> {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return None;
    }

    // `inherit` leaves inherited values in place (styles are inherited before declarations apply)
    if trimmed.eq_ignore_ascii_case("inherit") {
        return None;
    }

    // `initial`/`revert` -> reset to initial font values
    if trimmed.eq_ignore_ascii_case("initial") || trimmed.eq_ignore_ascii_case("revert") {
        let defaults = ComputedStyle::default();
        return Some((
            defaults.font_style,
            defaults.font_weight,
            defaults.font_variant,
            defaults.font_stretch,
            defaults.font_size,
            defaults.line_height.clone(),
            defaults.font_family.clone(),
        ));
    }

    let mut input = ParserInput::new(trimmed);
    let mut parser = Parser::new(&mut input);

    enum Phase {
        PreSize,
        AfterSize,
        AfterSlash,
    }

    let mut phase = Phase::PreSize;
    let mut font_style: Option<FontStyle> = None;
    let mut font_weight: Option<FontWeight> = None;
    let mut font_variant: Option<FontVariant> = None;
    let mut font_stretch: Option<FontStretch> = None;
    let mut font_size: Option<f32> = None;
    let mut line_height: Option<LineHeight> = None;
    let mut families: Vec<String> = Vec::new();
    let mut current_family: Vec<String> = Vec::new();

    while let Ok(token) = parser.next() {
        match phase {
            Phase::PreSize => {
                if matches!(token, Token::Delim('/')) {
                    return None; // slash before size is invalid
                }

                if font_size.is_none() {
                    if let Some(sz) = parse_font_size_token(&token, parent_font_size, root_font_size) {
                        font_size = Some(sz);
                        phase = Phase::AfterSize;
                        continue;
                    }
                }

                if let Token::Ident(ref ident) = token {
                    let ident = ident.as_ref();
                    match ident {
                        "normal" => {
                            if font_style.is_none() {
                                font_style = Some(FontStyle::Normal);
                            } else if font_weight.is_none() {
                                font_weight = Some(FontWeight::Normal);
                            } else if font_stretch.is_none() {
                                font_stretch = Some(FontStretch::Normal);
                            }
                        }
                        "small-caps" => font_variant = Some(FontVariant::SmallCaps),
                        "italic" => font_style = Some(FontStyle::Italic),
                        "oblique" => {
                            font_style = Some(FontStyle::Oblique(None));
                            if let Ok(Some(angle)) = parser.try_parse(|p| {
                                let t = p.next()?;
                                Ok::<_, cssparser::ParseError<()>>(parse_angle_token(&t))
                            }) {
                                font_style = Some(FontStyle::Oblique(Some(angle)));
                            }
                        }
                        "bold" => font_weight = Some(FontWeight::Bold),
                        "bolder" => font_weight = Some(FontWeight::Bolder),
                        "lighter" => font_weight = Some(FontWeight::Lighter),
                        _ => {
                            if font_stretch.is_none() {
                                if let Some(stretch) = parse_font_stretch_keyword(ident) {
                                    font_stretch = Some(stretch);
                                }
                            }
                            if font_variant.is_none() {
                                if ident == "normal" {
                                    font_variant = Some(FontVariant::Normal);
                                }
                            }
                        }
                    }
                } else if let Token::Number { value, .. } = token {
                    if font_weight.is_none() && *value >= 1.0 && *value <= 1000.0 {
                        font_weight = Some(FontWeight::Number((*value as u16).clamp(1, 1000)));
                    }
                } else if let Token::Percentage { unit_value, .. } = token {
                    if font_stretch.is_none() {
                        font_stretch = Some(FontStretch::from_percentage(*unit_value * 100.0));
                    }
                } else if let Token::Dimension { ref unit, .. } = token {
                    // Oblique angles are allowed; ignore them for now.
                    let u = unit.as_ref();
                    if matches!(u, "deg" | "grad" | "rad" | "turn") {
                        continue;
                    }
                }
            }
            Phase::AfterSlash => {
                if let Some(parsed) = parse_line_height_token(&token) {
                    line_height = Some(parsed);
                    phase = Phase::AfterSize;
                } else {
                    return None;
                }
            }
            Phase::AfterSize => match token {
                Token::Delim('/') => {
                    if line_height.is_some() {
                        return None; // multiple slashes not allowed
                    }
                    phase = Phase::AfterSlash;
                }
                Token::Comma => {
                    if !current_family.is_empty() {
                        families.push(current_family.join(" "));
                        current_family.clear();
                    }
                }
                Token::QuotedString(ref s) => {
                    if !current_family.is_empty() {
                        families.push(current_family.join(" "));
                        current_family.clear();
                    }
                    families.push(s.as_ref().to_string());
                }
                Token::Ident(ref ident) => {
                    current_family.push(ident.as_ref().to_string());
                }
                _ => {}
            },
        }
    }

    if matches!(phase, Phase::AfterSlash) && line_height.is_none() {
        return None;
    }

    if !current_family.is_empty() {
        families.push(current_family.join(" "));
    }

    if font_size.is_none() || families.is_empty() {
        return None;
    }

    Some((
        font_style.unwrap_or(FontStyle::Normal),
        font_weight.unwrap_or(FontWeight::Normal),
        font_variant.unwrap_or(FontVariant::Normal),
        font_stretch.unwrap_or(FontStretch::Normal),
        font_size.unwrap_or(parent_font_size),
        line_height.unwrap_or(LineHeight::Normal),
        families,
    ))
}

fn parse_font_size_token(token: &Token, parent_font_size: f32, root_font_size: f32) -> Option<f32> {
    if let Token::Ident(ref ident) = token {
        if let Some(size) = parse_font_size_keyword(ident.as_ref(), parent_font_size) {
            return Some(size);
        }
    }

    if let Some(len) = length_from_token(token) {
        return resolve_font_size_length(len, parent_font_size, root_font_size);
    }

    None
}

fn parse_line_height_token(token: &Token) -> Option<LineHeight> {
    match token {
        Token::Ident(ref ident) if ident.as_ref().eq_ignore_ascii_case("normal") => Some(LineHeight::Normal),
        Token::Number { value, .. } => Some(LineHeight::Number(*value)),
        Token::Percentage { unit_value, .. } => Some(LineHeight::Number(*unit_value)),
        _ => length_from_token(token).map(LineHeight::Length),
    }
}

fn parse_font_size_keyword(keyword: &str, parent_font_size: f32) -> Option<f32> {
    let scale: f32 = 1.2;
    let medium = 16.0;

    match keyword {
        "xx-small" => Some(medium / scale.powi(3)),
        "x-small" => Some(medium / scale.powi(2)),
        "small" => Some(medium / scale),
        "medium" => Some(medium),
        "large" => Some(medium * scale),
        "x-large" => Some(medium * scale.powi(2)),
        "xx-large" => Some(medium * scale.powi(3)),
        "xxx-large" => Some(medium * scale.powi(4)),
        "larger" => Some(parent_font_size * scale),
        "smaller" => Some(parent_font_size / scale),
        _ => None,
    }
}

fn resolve_font_size_length(len: Length, parent_font_size: f32, root_font_size: f32) -> Option<f32> {
    if len.unit.is_absolute() {
        return Some(len.to_px());
    }
    if len.unit == LengthUnit::Em {
        return Some(len.value * parent_font_size);
    }
    if len.unit == LengthUnit::Rem {
        return Some(len.value * root_font_size);
    }
    if len.unit == LengthUnit::Percent {
        return Some((len.value / 100.0) * parent_font_size);
    }
    if matches!(
        len.unit,
        LengthUnit::Vw | LengthUnit::Vh | LengthUnit::Vmin | LengthUnit::Vmax
    ) {
        // Viewport units for font-size resolve to pixels directly
        return Some(len.to_px());
    }
    None
}

fn length_from_token(token: &Token) -> Option<Length> {
    match token {
        Token::Dimension { value, ref unit, .. } => match unit.as_ref() {
            "px" => Some(Length::px(*value)),
            "em" => Some(Length::em(*value)),
            "rem" => Some(Length::rem(*value)),
            "pt" => Some(Length::pt(*value)),
            "pc" => Some(Length::pc(*value)),
            "in" => Some(Length::inches(*value)),
            "cm" => Some(Length::cm(*value)),
            "mm" => Some(Length::mm(*value)),
            "vh" => Some(Length {
                value: *value,
                unit: LengthUnit::Vh,
            }),
            "vw" => Some(Length {
                value: *value,
                unit: LengthUnit::Vw,
            }),
            "vmin" => Some(Length {
                value: *value,
                unit: LengthUnit::Vmin,
            }),
            "vmax" => Some(Length {
                value: *value,
                unit: LengthUnit::Vmax,
            }),
            "%" => Some(Length::percent(*value)),
            _ => None,
        },
        Token::Percentage { unit_value, .. } => Some(Length::percent(*unit_value * 100.0)),
        Token::Number { value, .. } if *value == 0.0 => Some(Length::px(0.0)),
        _ => None,
    }
}

fn parse_angle_degrees<'i, 't>(input: &mut Parser<'i, 't>) -> Result<f32, cssparser::ParseError<'i, ()>> {
    let location = input.current_source_location();
    match input.next()? {
        Token::Dimension { value, ref unit, .. } => match unit.as_ref() {
            "deg" => Ok(*value),
            "grad" => Ok(*value * 0.9),
            "turn" => Ok(*value * 360.0),
            "rad" => Ok(*value * (180.0 / std::f32::consts::PI)),
            _ => Err(location.new_custom_error(())),
        },
        Token::Number { value, .. } if *value == 0.0 => Ok(0.0),
        _ => Err(location.new_custom_error(())),
    }
}

fn parse_length_component<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Length, cssparser::ParseError<'i, ()>> {
    let location = input.current_source_location();
    match input.next()? {
        Token::Dimension { value, ref unit, .. } => {
            let unit = unit.as_ref().to_ascii_lowercase();
            let len = match unit.as_str() {
                "px" => Length::px(*value),
                "em" => Length::em(*value),
                "rem" => Length::rem(*value),
                "ex" => Length::ex(*value),
                "ch" => Length::ch(*value),
                "pt" => Length::pt(*value),
                "pc" => Length::pc(*value),
                "in" => Length::inches(*value),
                "cm" => Length::cm(*value),
                "mm" => Length::mm(*value),
                "q" => Length::q(*value),
                "vw" => Length::new(*value, LengthUnit::Vw),
                "vh" => Length::new(*value, LengthUnit::Vh),
                "vmin" => Length::new(*value, LengthUnit::Vmin),
                "vmax" => Length::new(*value, LengthUnit::Vmax),
                _ => return Err(location.new_custom_error(())),
            };
            Ok(len)
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => {
            crate::css::properties::parse_calc_function_length(input)
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("min") => {
            crate::css::properties::parse_min_max_function_length(input, crate::css::properties::MathFn::Min)
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("max") => {
            crate::css::properties::parse_min_max_function_length(input, crate::css::properties::MathFn::Max)
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("clamp") => {
            crate::css::properties::parse_clamp_function_length(input)
        }
        Token::Percentage { unit_value, .. } => Ok(Length::percent(*unit_value * 100.0)),
        Token::Number { value, .. } if *value == 0.0 => Ok(Length::px(0.0)),
        _ => Err(location.new_custom_error(())),
    }
}

fn parse_css_color_value<'i, 't>(input: &mut Parser<'i, 't>) -> Result<FilterColor, cssparser::ParseError<'i, ()>> {
    let location = input.current_source_location();
    let token = input.next()?;
    let raw = match token {
        Token::Ident(ref ident) => ident.as_ref().to_string(),
        Token::Hash(ref value) | Token::IDHash(ref value) => format!("#{}", value),
        Token::Function(ref name) => {
            let func = name.as_ref().to_string();
            let inner = input.parse_nested_block(|block| Ok(block.slice_from(block.position()).to_string()))?;
            format!("{}({})", func, inner)
        }
        _ => return Err(location.new_custom_error(())),
    };

    if raw.eq_ignore_ascii_case("currentcolor") {
        return Ok(FilterColor::CurrentColor);
    }

    let parsed = crate::style::color::Color::parse(&raw).map_err(|_| location.new_custom_error(()))?;
    Ok(FilterColor::Color(parsed.to_rgba(Rgba::BLACK)))
}

fn parse_drop_shadow<'i, 't>(input: &mut Parser<'i, 't>) -> Result<FilterFunction, cssparser::ParseError<'i, ()>> {
    let mut lengths = Vec::new();
    let mut color: Option<FilterColor> = None;

    while !input.is_exhausted() {
        input.skip_whitespace();
        if input.is_exhausted() {
            break;
        }

        if color.is_none() {
            if let Ok(c) = input.try_parse(parse_css_color_value) {
                color = Some(c);
                continue;
            }
        }

        if let Ok(len) = input.try_parse(parse_length_component) {
            lengths.push(len);
            continue;
        }

        // Unexpected token
        let _ = input.next();
        return Err(input.new_custom_error(()));
    }

    if lengths.len() < 2 {
        return Err(input.new_custom_error(()));
    }

    let blur = lengths.get(2).copied().unwrap_or_else(|| Length::px(0.0));
    let spread = lengths.get(3).copied().unwrap_or_else(|| Length::px(0.0));

    Ok(FilterFunction::DropShadow(FilterShadow {
        offset_x: lengths[0],
        offset_y: lengths[1],
        blur_radius: blur,
        spread,
        color: color.unwrap_or(FilterColor::CurrentColor),
    }))
}

fn parse_mix_blend_mode(kw: &str) -> Option<MixBlendMode> {
    match kw {
        "normal" => Some(MixBlendMode::Normal),
        "multiply" => Some(MixBlendMode::Multiply),
        "screen" => Some(MixBlendMode::Screen),
        "overlay" => Some(MixBlendMode::Overlay),
        "darken" => Some(MixBlendMode::Darken),
        "lighten" => Some(MixBlendMode::Lighten),
        "color-dodge" => Some(MixBlendMode::ColorDodge),
        "color-burn" => Some(MixBlendMode::ColorBurn),
        "hard-light" => Some(MixBlendMode::HardLight),
        "soft-light" => Some(MixBlendMode::SoftLight),
        "difference" => Some(MixBlendMode::Difference),
        "exclusion" => Some(MixBlendMode::Exclusion),
        "hue" => Some(MixBlendMode::Hue),
        "saturation" => Some(MixBlendMode::Saturation),
        "color" => Some(MixBlendMode::Color),
        "luminosity" => Some(MixBlendMode::Luminosity),
        _ => None,
    }
}

fn parse_background_box(value: &PropertyValue) -> Option<BackgroundBox> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "border-box" => Some(BackgroundBox::BorderBox),
            "padding-box" => Some(BackgroundBox::PaddingBox),
            "content-box" => Some(BackgroundBox::ContentBox),
            _ => None,
        },
        _ => None,
    }
}

fn parse_repeat_keyword(kw: &str) -> Option<BackgroundRepeatKeyword> {
    match kw {
        "repeat" => Some(BackgroundRepeatKeyword::Repeat),
        "space" => Some(BackgroundRepeatKeyword::Space),
        "round" => Some(BackgroundRepeatKeyword::Round),
        "no-repeat" => Some(BackgroundRepeatKeyword::NoRepeat),
        _ => None,
    }
}

fn parse_background_repeat(value: &PropertyValue) -> Option<BackgroundRepeat> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "repeat-x" => Some(BackgroundRepeat::repeat_x()),
            "repeat-y" => Some(BackgroundRepeat::repeat_y()),
            _ => parse_repeat_keyword(kw).map(|k| BackgroundRepeat { x: k, y: k }),
        },
        PropertyValue::Multiple(values) if values.len() == 2 => {
            if let (PropertyValue::Keyword(x_kw), PropertyValue::Keyword(y_kw)) = (&values[0], &values[1]) {
                let x = parse_repeat_keyword(x_kw)?;
                let y = parse_repeat_keyword(y_kw)?;
                Some(BackgroundRepeat { x, y })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn parse_background_position(value: &PropertyValue) -> Option<BackgroundPosition> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum AxisKind {
        Horizontal,
        Vertical,
        Either, // center
    }

    #[derive(Clone, Copy)]
    enum Part {
        Keyword(AxisKind, f32),
        Offset(Length),
    }

    fn classify(value: &PropertyValue) -> Option<Part> {
        match value {
            PropertyValue::Keyword(kw) => match kw.as_str() {
                "left" => Some(Part::Keyword(AxisKind::Horizontal, 0.0)),
                "right" => Some(Part::Keyword(AxisKind::Horizontal, 1.0)),
                "top" => Some(Part::Keyword(AxisKind::Vertical, 0.0)),
                "bottom" => Some(Part::Keyword(AxisKind::Vertical, 1.0)),
                "center" => Some(Part::Keyword(AxisKind::Either, 0.5)),
                _ => None,
            },
            PropertyValue::Length(l) => Some(Part::Offset(*l)),
            PropertyValue::Percentage(p) => Some(Part::Offset(Length::percent(*p))),
            PropertyValue::Number(n) if *n == 0.0 => Some(Part::Offset(Length::px(0.0))),
            _ => None,
        }
    }

    fn component_from_keyword(align: f32, offset: Option<Length>) -> BackgroundPositionComponent {
        let mut off = offset.unwrap_or_else(|| Length::px(0.0));
        if (align - 1.0).abs() < 1e-6 {
            off.value = -off.value;
        }
        BackgroundPositionComponent {
            alignment: align,
            offset: off,
        }
    }

    fn component_from_single(part: &Part, axis: AxisKind) -> Option<BackgroundPositionComponent> {
        match (part, axis) {
            (Part::Keyword(kind, align), _) if *kind == AxisKind::Either => Some(component_from_keyword(*align, None)),
            (Part::Keyword(kind, align), AxisKind::Horizontal) if *kind == AxisKind::Horizontal => {
                Some(component_from_keyword(*align, None))
            }
            (Part::Keyword(kind, align), AxisKind::Vertical) if *kind == AxisKind::Vertical => {
                Some(component_from_keyword(*align, None))
            }
            (Part::Offset(len), AxisKind::Horizontal | AxisKind::Vertical) => {
                Some(component_from_keyword(0.0, Some(*len)))
            }
            _ => None,
        }
    }

    let parts: Vec<Part> = match value {
        PropertyValue::Multiple(values) if !values.is_empty() => values.iter().filter_map(classify).collect(),
        other => classify(other).into_iter().collect(),
    };

    if parts.is_empty() || parts.len() > 4 {
        return None;
    }

    let mut x: Option<BackgroundPositionComponent> = None;
    let mut y: Option<BackgroundPositionComponent> = None;

    match parts.len() {
        1 => match parts[0] {
            Part::Keyword(AxisKind::Horizontal, align) => {
                x = Some(component_from_keyword(align, None));
                y = Some(component_from_keyword(0.5, None));
            }
            Part::Keyword(AxisKind::Vertical, align) => {
                y = Some(component_from_keyword(align, None));
                x = Some(component_from_keyword(0.5, None));
            }
            Part::Keyword(AxisKind::Either, align) => {
                x = Some(component_from_keyword(align, None));
                y = Some(component_from_keyword(align, None));
            }
            Part::Offset(len) => {
                x = Some(component_from_keyword(0.0, Some(len)));
                y = Some(component_from_keyword(0.5, None));
            }
        },
        2 => {
            // Two-value syntax: first is horizontal, second vertical; if order is vertical+horizontal, swap.
            let (first, second) = (&parts[0], &parts[1]);

            let first_is_vertical = matches!(first, Part::Keyword(AxisKind::Vertical, _));
            let second_is_horizontal = matches!(second, Part::Keyword(AxisKind::Horizontal, _));
            // center + horizontal keyword => treat first as vertical center, second as horizontal
            if matches!(first, Part::Keyword(AxisKind::Either, _)) && second_is_horizontal {
                x = component_from_single(second, AxisKind::Horizontal);
                y = Some(component_from_keyword(0.5, None));
            // center + vertical keyword => treat first as horizontal center, second as vertical
            } else if matches!(first, Part::Keyword(AxisKind::Either, _))
                && matches!(second, Part::Keyword(AxisKind::Vertical, _))
            {
                x = Some(component_from_keyword(0.5, None));
                y = component_from_single(second, AxisKind::Vertical);
            } else if first_is_vertical && second_is_horizontal {
                y = component_from_single(first, AxisKind::Vertical);
                x = component_from_single(second, AxisKind::Horizontal);
            } else {
                let first_h = component_from_single(first, AxisKind::Horizontal);
                let first_v = component_from_single(first, AxisKind::Vertical);
                let second_h = component_from_single(second, AxisKind::Horizontal);
                let second_v = component_from_single(second, AxisKind::Vertical);

                // Ambiguous center handling: if first is center and second is a horizontal-only keyword,
                // treat the first as vertical center and second as horizontal (center left -> left center).
                if first_v.is_none() && first_h.is_some() && second_h.is_some() && second_v.is_none() {
                    x = second_h;
                    y = first_v.or_else(|| Some(component_from_keyword(0.5, None)));
                } else if first_v.is_some() && second_h.is_none() && second_v.is_some() {
                    // first vertical only, second vertical or center -> treat first vertical, second becomes horizontal center.
                    y = first_v;
                    x = second_h.or_else(|| Some(component_from_keyword(0.5, None)));
                } else {
                    x = first_h.or_else(|| second_h);
                    y = second_v.or_else(|| first_v);
                }
            }
            if x.is_none() {
                x = Some(component_from_keyword(0.5, None));
            }
            if y.is_none() {
                y = Some(component_from_keyword(0.5, None));
            }
        }
        3 => {
            // One axis has keyword + offset, the other a single component.
            let a = &parts[0];
            let b = &parts[1];
            let c = &parts[2];

            // Horizontal pair first
            if matches!(a, Part::Keyword(AxisKind::Horizontal | AxisKind::Either, _)) && matches!(b, Part::Offset(_)) {
                if let Part::Keyword(_, align) = a {
                    x = Some(component_from_keyword(
                        *align,
                        Some(match b {
                            Part::Offset(l) => *l,
                            _ => unreachable!(),
                        }),
                    ));
                } else if let Part::Offset(_) = a {
                    // unreachable
                }
                y = component_from_single(c, AxisKind::Vertical);
            }

            // Vertical pair first
            if x.is_none()
                && matches!(a, Part::Keyword(AxisKind::Vertical | AxisKind::Either, _))
                && matches!(b, Part::Offset(_))
            {
                if let Part::Keyword(_, align) = a {
                    y = Some(component_from_keyword(
                        *align,
                        Some(match b {
                            Part::Offset(l) => *l,
                            _ => unreachable!(),
                        }),
                    ));
                }
                x = component_from_single(c, AxisKind::Horizontal);
            }
        }
        4 => {
            let a = &parts[0];
            let b = &parts[1];
            let c = &parts[2];
            let d = &parts[3];

            // horizontal pair then vertical pair
            if matches!(a, Part::Keyword(AxisKind::Horizontal | AxisKind::Either, _))
                && matches!(b, Part::Offset(_))
                && matches!(c, Part::Keyword(AxisKind::Vertical | AxisKind::Either, _))
                && matches!(d, Part::Offset(_))
            {
                if let Part::Keyword(_, align) = a {
                    x = Some(component_from_keyword(
                        *align,
                        Some(match b {
                            Part::Offset(l) => *l,
                            _ => unreachable!(),
                        }),
                    ));
                }
                if let Part::Keyword(_, align) = c {
                    y = Some(component_from_keyword(
                        *align,
                        Some(match d {
                            Part::Offset(l) => *l,
                            _ => unreachable!(),
                        }),
                    ));
                }
            } else if matches!(a, Part::Keyword(AxisKind::Vertical | AxisKind::Either, _))
                && matches!(b, Part::Offset(_))
                && matches!(c, Part::Keyword(AxisKind::Horizontal | AxisKind::Either, _))
                && matches!(d, Part::Offset(_))
            {
                if let Part::Keyword(_, align) = a {
                    y = Some(component_from_keyword(
                        *align,
                        Some(match b {
                            Part::Offset(l) => *l,
                            _ => unreachable!(),
                        }),
                    ));
                }
                if let Part::Keyword(_, align) = c {
                    x = Some(component_from_keyword(
                        *align,
                        Some(match d {
                            Part::Offset(l) => *l,
                            _ => unreachable!(),
                        }),
                    ));
                }
            }
        }
        _ => {}
    }

    let x = x.unwrap_or_else(|| component_from_keyword(0.5, None));
    let y = y.unwrap_or_else(|| component_from_keyword(0.5, None));

    Some(BackgroundPosition::Position { x, y })
}

fn parse_background_position_component_x(value: &PropertyValue) -> Option<BackgroundPositionComponent> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "left" => Some(BackgroundPositionComponent {
                alignment: 0.0,
                offset: Length::px(0.0),
            }),
            "right" => Some(BackgroundPositionComponent {
                alignment: 1.0,
                offset: Length::px(0.0),
            }),
            "center" => Some(BackgroundPositionComponent {
                alignment: 0.5,
                offset: Length::px(0.0),
            }),
            _ => None,
        },
        PropertyValue::Length(len) => Some(BackgroundPositionComponent {
            alignment: 0.0,
            offset: *len,
        }),
        PropertyValue::Percentage(p) => Some(BackgroundPositionComponent {
            alignment: 0.0,
            offset: Length::percent(*p),
        }),
        PropertyValue::Number(n) if *n == 0.0 => Some(BackgroundPositionComponent {
            alignment: 0.0,
            offset: Length::px(0.0),
        }),
        _ => None,
    }
}

fn parse_background_position_component_y(value: &PropertyValue) -> Option<BackgroundPositionComponent> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "top" => Some(BackgroundPositionComponent {
                alignment: 0.0,
                offset: Length::px(0.0),
            }),
            "bottom" => Some(BackgroundPositionComponent {
                alignment: 1.0,
                offset: Length::px(0.0),
            }),
            "center" => Some(BackgroundPositionComponent {
                alignment: 0.5,
                offset: Length::px(0.0),
            }),
            _ => None,
        },
        PropertyValue::Length(len) => Some(BackgroundPositionComponent {
            alignment: 0.0,
            offset: *len,
        }),
        PropertyValue::Percentage(p) => Some(BackgroundPositionComponent {
            alignment: 0.0,
            offset: Length::percent(*p),
        }),
        PropertyValue::Number(n) if *n == 0.0 => Some(BackgroundPositionComponent {
            alignment: 0.0,
            offset: Length::px(0.0),
        }),
        _ => None,
    }
}

fn parse_text_decoration_line(value: &PropertyValue) -> Option<TextDecorationLine> {
    let components: Vec<&PropertyValue> = match value {
        PropertyValue::Multiple(values) if !values.is_empty() => values.iter().collect(),
        _ => vec![value],
    };

    let mut lines = TextDecorationLine::NONE;
    let mut saw_none = false;

    for comp in components {
        if let PropertyValue::Keyword(kw) = comp {
            match kw.as_str() {
                "none" => {
                    saw_none = true;
                    lines = TextDecorationLine::NONE;
                    break;
                }
                "underline" => lines.insert(TextDecorationLine::UNDERLINE),
                "overline" => lines.insert(TextDecorationLine::OVERLINE),
                "line-through" => lines.insert(TextDecorationLine::LINE_THROUGH),
                _ => {}
            }
        }
    }

    if saw_none {
        Some(TextDecorationLine::NONE)
    } else if lines.is_empty() {
        None
    } else {
        Some(lines)
    }
}

fn parse_text_decoration_style(value: &PropertyValue) -> Option<TextDecorationStyle> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "solid" => Some(TextDecorationStyle::Solid),
            "double" => Some(TextDecorationStyle::Double),
            "dotted" => Some(TextDecorationStyle::Dotted),
            "dashed" => Some(TextDecorationStyle::Dashed),
            "wavy" => Some(TextDecorationStyle::Wavy),
            _ => None,
        },
        _ => None,
    }
}

fn parse_text_decoration_color(value: &PropertyValue) -> Option<Option<Rgba>> {
    match value {
        PropertyValue::Color(c) => Some(Some(*c)),
        PropertyValue::Keyword(kw) if kw == "currentcolor" => Some(None),
        _ => None,
    }
}

fn parse_text_underline_offset(value: &PropertyValue) -> Option<TextUnderlineOffset> {
    match value {
        PropertyValue::Keyword(kw) if kw == "auto" => Some(TextUnderlineOffset::Auto),
        PropertyValue::Length(l) => Some(TextUnderlineOffset::Length(*l)),
        PropertyValue::Percentage(p) => Some(TextUnderlineOffset::Length(Length::percent(*p))),
        _ => None,
    }
}

fn parse_text_underline_position(value: &PropertyValue) -> Option<TextUnderlinePosition> {
    enum Side {
        Left,
        Right,
    }

    let mut under = false;
    let mut side: Option<Side> = None;
    let mut from_font = false;
    let mut auto = false;

    let mut handle_keyword = |kw: &str| -> Option<()> {
        match kw {
            "auto" => {
                if auto || from_font || under || side.is_some() {
                    return None;
                }
                auto = true;
            }
            "from-font" => {
                if auto || from_font || under || side.is_some() {
                    return None;
                }
                from_font = true;
            }
            "under" => {
                if auto || from_font || under {
                    return None;
                }
                under = true;
            }
            "left" => {
                if auto || from_font || side.is_some() {
                    return None;
                }
                side = Some(Side::Left);
            }
            "right" => {
                if auto || from_font || side.is_some() {
                    return None;
                }
                side = Some(Side::Right);
            }
            _ => return None,
        }
        Some(())
    };

    let keywords: Vec<String> = match value {
        PropertyValue::Multiple(values) => {
            let mut kws = Vec::new();
            for v in values {
                if let PropertyValue::Keyword(kw) = v {
                    kws.extend(kw.split_whitespace().map(|s| s.to_string()));
                } else {
                    return None;
                }
            }
            kws
        }
        PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
        _ => return None,
    };

    if keywords.is_empty() {
        return None;
    }

    for kw in keywords {
        handle_keyword(&kw)?;
    }

    if auto {
        Some(TextUnderlinePosition::Auto)
    } else if from_font {
        Some(TextUnderlinePosition::FromFont)
    } else {
        match (under, side) {
            (true, Some(Side::Left)) => Some(TextUnderlinePosition::UnderLeft),
            (true, Some(Side::Right)) => Some(TextUnderlinePosition::UnderRight),
            (true, None) => Some(TextUnderlinePosition::Under),
            (false, Some(Side::Left)) => Some(TextUnderlinePosition::Left),
            (false, Some(Side::Right)) => Some(TextUnderlinePosition::Right),
            (false, None) => None,
        }
    }
}

fn parse_text_emphasis_style(value: &PropertyValue) -> Option<TextEmphasisStyle> {
    fn parse_keywords(values: &[&str]) -> Option<TextEmphasisStyle> {
        let mut fill = TextEmphasisFill::Filled;
        let mut shape: Option<TextEmphasisShape> = None;

        for kw in values {
            match *kw {
                "filled" => fill = TextEmphasisFill::Filled,
                "open" => fill = TextEmphasisFill::Open,
                "dot" => shape = Some(TextEmphasisShape::Dot),
                "circle" => shape = Some(TextEmphasisShape::Circle),
                "double-circle" => shape = Some(TextEmphasisShape::DoubleCircle),
                "triangle" => shape = Some(TextEmphasisShape::Triangle),
                "sesame" => shape = Some(TextEmphasisShape::Sesame),
                "none" => return Some(TextEmphasisStyle::None),
                _ => return None,
            }
        }

        shape.map(|s| TextEmphasisStyle::Mark { fill, shape: s })
    }

    match value {
        PropertyValue::Keyword(kw) => {
            let parts: Vec<&str> = kw.split_whitespace().collect();
            parse_keywords(&parts)
        }
        PropertyValue::Multiple(values) => {
            let mut parts = Vec::new();
            for v in values {
                match v {
                    PropertyValue::Keyword(kw) => {
                        parts.extend(kw.split_whitespace());
                    }
                    PropertyValue::String(s) if !s.is_empty() => {
                        return Some(TextEmphasisStyle::String(s.clone()));
                    }
                    _ => return None,
                }
            }
            parse_keywords(&parts)
        }
        PropertyValue::String(s) if !s.is_empty() => Some(TextEmphasisStyle::String(s.clone())),
        _ => None,
    }
}

fn parse_text_emphasis_color(value: &PropertyValue) -> Option<Option<Rgba>> {
    match value {
        PropertyValue::Color(c) => Some(Some(*c)),
        PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => Some(None),
        PropertyValue::Keyword(kw) => match crate::style::color::Color::parse(kw) {
            Ok(crate::style::color::Color::Rgba(c)) => Some(Some(c)),
            Ok(crate::style::color::Color::CurrentColor) => Some(None),
            _ => None,
        },
        _ => None,
    }
}

fn parse_text_emphasis_position(value: &PropertyValue) -> Option<TextEmphasisPosition> {
    #[derive(Default)]
    struct PositionParse {
        over_under: Option<bool>, // true over, false under
        side: Option<bool>,       // true right, false left
    }

    let mut state = PositionParse::default();

    let handle_kw = |kw: &str, state: &mut PositionParse| -> bool {
        match kw {
            "over" => {
                if state.over_under.is_some() {
                    return false;
                }
                state.over_under = Some(true);
            }
            "under" => {
                if state.over_under.is_some() {
                    return false;
                }
                state.over_under = Some(false);
            }
            "left" => {
                if state.side.is_some() {
                    return false;
                }
                state.side = Some(false);
            }
            "right" => {
                if state.side.is_some() {
                    return false;
                }
                state.side = Some(true);
            }
            "auto" => {
                if state.over_under.is_some() || state.side.is_some() {
                    return false;
                }
                return true;
            }
            _ => return false,
        }
        true
    };

    let keywords: Vec<String> = match value {
        PropertyValue::Multiple(values) => {
            let mut kws = Vec::new();
            for v in values {
                if let PropertyValue::Keyword(kw) = v {
                    kws.extend(kw.split_whitespace().map(|s| s.to_string()));
                } else {
                    return None;
                }
            }
            kws
        }
        PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
        _ => return None,
    };

    if keywords.is_empty() {
        return None;
    }

    for kw in &keywords {
        if !handle_kw(kw, &mut state) {
            return None;
        }
    }

    match (state.over_under, state.side) {
        (None, None) => Some(TextEmphasisPosition::Auto),
        (Some(true), None) => Some(TextEmphasisPosition::Over),
        (Some(false), None) => Some(TextEmphasisPosition::Under),
        (Some(true), Some(false)) => Some(TextEmphasisPosition::OverLeft),
        (Some(true), Some(true)) => Some(TextEmphasisPosition::OverRight),
        (Some(false), Some(false)) => Some(TextEmphasisPosition::UnderLeft),
        (Some(false), Some(true)) => Some(TextEmphasisPosition::UnderRight),
        (None, Some(false)) => Some(TextEmphasisPosition::OverLeft),
        (None, Some(true)) => Some(TextEmphasisPosition::OverRight),
    }
}

fn parse_text_emphasis_shorthand(value: &PropertyValue) -> Option<(Option<TextEmphasisStyle>, Option<Option<Rgba>>)> {
    let values: Vec<&PropertyValue> = match value {
        PropertyValue::Multiple(vals) => vals.iter().collect(),
        other => vec![other],
    };

    if values.is_empty() {
        return None;
    }

    let mut style: Option<TextEmphasisStyle> = None;
    let mut color: Option<Option<Rgba>> = None;

    for v in values {
        if color.is_none() {
            if let Some(c) = parse_text_emphasis_color(v) {
                color = Some(c);
                continue;
            }
        }
        if style.is_none() {
            if let Some(s) = parse_text_emphasis_style(v) {
                style = Some(s);
                continue;
            }
        }
    }

    Some((style, color))
}

fn parse_text_decoration_thickness(
    value: &PropertyValue,
    _parent_font_size: f32,
    _root_font_size: f32,
) -> Option<TextDecorationThickness> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "auto" => Some(TextDecorationThickness::Auto),
            "from-font" => Some(TextDecorationThickness::FromFont),
            _ => None,
        },
        PropertyValue::Length(l) => Some(TextDecorationThickness::Length(*l)),
        PropertyValue::Percentage(p) => Some(TextDecorationThickness::Length(Length::percent(*p))),
        _ => None,
    }
}

fn parse_text_decoration_skip_ink(value: &PropertyValue) -> Option<TextDecorationSkipInk> {
    if let PropertyValue::Keyword(kw) = value {
        return match kw.as_str() {
            "auto" => Some(TextDecorationSkipInk::Auto),
            "none" => Some(TextDecorationSkipInk::None),
            "all" => Some(TextDecorationSkipInk::All),
            _ => None,
        };
    }
    None
}

fn parse_text_combine_upright(value: &PropertyValue) -> Option<TextCombineUpright> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "none" => Some(TextCombineUpright::None),
            "all" => Some(TextCombineUpright::All),
            "digits" => Some(TextCombineUpright::Digits(2)),
            _ => None,
        },
        PropertyValue::Multiple(values) => {
            if values.is_empty() {
                return None;
            }
            if let PropertyValue::Keyword(first) = &values[0] {
                if first == "digits" {
                    let count = values.get(1).and_then(|v| match v {
                        PropertyValue::Number(n) => Some(*n as i32),
                        _ => None,
                    });
                    let clamped = count.unwrap_or(2).clamp(1, 4) as u8;
                    return Some(TextCombineUpright::Digits(clamped));
                }
            }
            if values.len() == 1 {
                return parse_text_combine_upright(&values[0]);
            }
            None
        }
        _ => None,
    }
}

fn parse_text_transform(value: &PropertyValue) -> Option<TextTransform> {
    use crate::style::types::CaseTransform;

    let mut case = CaseTransform::None;
    let mut full_width = false;
    let mut full_size_kana = false;

    let mut apply_keyword = |kw: &str| -> Option<()> {
        match kw {
            "none" => {
                // none must be the only keyword present
                case = CaseTransform::None;
                full_width = false;
                full_size_kana = false;
            }
            "uppercase" => {
                if !matches!(case, CaseTransform::None | CaseTransform::Uppercase) {
                    return None;
                }
                case = CaseTransform::Uppercase;
            }
            "lowercase" => {
                if !matches!(case, CaseTransform::None | CaseTransform::Lowercase) {
                    return None;
                }
                case = CaseTransform::Lowercase;
            }
            "capitalize" => {
                if !matches!(case, CaseTransform::None | CaseTransform::Capitalize) {
                    return None;
                }
                case = CaseTransform::Capitalize;
            }
            "full-width" => full_width = true,
            "full-size-kana" => full_size_kana = true,
            _ => return None,
        }
        Some(())
    };

    match value {
        PropertyValue::Keyword(kw) => apply_keyword(kw)?,
        PropertyValue::Multiple(list) if !list.is_empty() => {
            // 'none' cannot be combined with other keywords
            let has_none = list
                .iter()
                .any(|v| matches!(v, PropertyValue::Keyword(k) if k == "none"));
            if has_none && list.len() > 1 {
                return None;
            }
            for part in list {
                if let PropertyValue::Keyword(kw) = part {
                    apply_keyword(kw)?;
                } else {
                    return None;
                }
            }
        }
        _ => return None,
    }

    Some(TextTransform {
        case,
        full_width,
        full_size_kana,
    })
}

fn parse_list_style_type(value: &PropertyValue) -> Option<ListStyleType> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "disc" => Some(ListStyleType::Disc),
            "circle" => Some(ListStyleType::Circle),
            "square" => Some(ListStyleType::Square),
            "decimal" => Some(ListStyleType::Decimal),
            "decimal-leading-zero" => Some(ListStyleType::DecimalLeadingZero),
            "lower-roman" => Some(ListStyleType::LowerRoman),
            "upper-roman" => Some(ListStyleType::UpperRoman),
            "lower-alpha" | "lower-latin" => Some(ListStyleType::LowerAlpha),
            "upper-alpha" | "upper-latin" => Some(ListStyleType::UpperAlpha),
            "armenian" | "upper-armenian" => Some(ListStyleType::Armenian),
            "lower-armenian" => Some(ListStyleType::LowerArmenian),
            "georgian" => Some(ListStyleType::Georgian),
            "lower-greek" => Some(ListStyleType::LowerGreek),
            "none" => Some(ListStyleType::None),
            _ => None,
        },
        _ => None,
    }
}

fn parse_list_style_position(value: &PropertyValue) -> Option<ListStylePosition> {
    match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "inside" => Some(ListStylePosition::Inside),
            "outside" => Some(ListStylePosition::Outside),
            _ => None,
        },
        _ => None,
    }
}

fn parse_list_style_image(value: &PropertyValue) -> Option<ListStyleImage> {
    match value {
        PropertyValue::Keyword(kw) if kw == "none" => Some(ListStyleImage::None),
        PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => parse_image_set(kw)
            .and_then(|img| match img {
                BackgroundImage::Url(url) => Some(ListStyleImage::Url(url)),
                _ => None,
            }),
        PropertyValue::Url(url) => Some(ListStyleImage::Url(url.clone())),
        _ => None,
    }
}

#[derive(Clone, Copy)]
enum CounterPropertyKind {
    Reset,
    Increment,
    Set,
}

fn parse_counter_property(value: &PropertyValue, kind: CounterPropertyKind) -> Option<CounterSet> {
    let input = counter_value_to_string(value)?;
    let parsed = match kind {
        CounterPropertyKind::Reset => CounterSet::parse_reset(&input)?,
        CounterPropertyKind::Increment => CounterSet::parse_increment(&input)?,
        CounterPropertyKind::Set => CounterSet::parse_set(&input)?,
    };

    Some(parsed)
}

fn counter_value_to_string(value: &PropertyValue) -> Option<String> {
    match value {
        PropertyValue::Keyword(kw) => Some(kw.trim().to_string()),
        PropertyValue::Multiple(parts) => {
            let mut tokens = Vec::new();
            for part in parts {
                match part {
                    PropertyValue::Keyword(kw) => tokens.push(kw.clone()),
                    PropertyValue::Number(num) if num.fract().abs() < f32::EPSILON => {
                        tokens.push((*num as i32).to_string())
                    }
                    _ => return None,
                }
            }
            if tokens.is_empty() {
                None
            } else {
                Some(tokens.join(" "))
            }
        }
        _ => None,
    }
}

pub fn extract_margin_values(value: &PropertyValue) -> Option<Vec<Option<Length>>> {
    match value {
        PropertyValue::Length(len) => Some(vec![Some(*len)]),
        PropertyValue::Keyword(kw) if kw == "auto" => Some(vec![None]), // auto margins
        PropertyValue::Multiple(values) => {
            let lengths: Vec<Option<Length>> = values.iter().map(extract_length).collect();
            if lengths.is_empty() {
                None
            } else {
                Some(lengths)
            }
        }
        _ => None,
    }
}

pub fn extract_box_values(value: &PropertyValue) -> Option<Vec<Length>> {
    match value {
        PropertyValue::Length(len) => Some(vec![*len]),
        PropertyValue::Multiple(values) => {
            let lengths: Vec<Length> = values.iter().filter_map(extract_length).collect();
            if lengths.is_empty() {
                None
            } else {
                Some(lengths)
            }
        }
        _ => None,
    }
}

pub fn apply_margin_values(
    top: &mut Option<Length>,
    right: &mut Option<Length>,
    bottom: &mut Option<Length>,
    left: &mut Option<Length>,
    values: Vec<Option<Length>>,
) {
    match values.len() {
        1 => {
            *top = values[0];
            *right = values[0];
            *bottom = values[0];
            *left = values[0];
        }
        2 => {
            *top = values[0];
            *bottom = values[0];
            *right = values[1];
            *left = values[1];
        }
        3 => {
            *top = values[0];
            *right = values[1];
            *left = values[1];
            *bottom = values[2];
        }
        4 => {
            *top = values[0];
            *right = values[1];
            *bottom = values[2];
            *left = values[3];
        }
        _ => {}
    }
}

pub fn apply_box_values(
    top: &mut Length,
    right: &mut Length,
    bottom: &mut Length,
    left: &mut Length,
    values: Vec<Length>,
) {
    match values.len() {
        1 => {
            *top = values[0];
            *right = values[0];
            *bottom = values[0];
            *left = values[0];
        }
        2 => {
            *top = values[0];
            *bottom = values[0];
            *right = values[1];
            *left = values[1];
        }
        3 => {
            *top = values[0];
            *right = values[1];
            *left = values[1];
            *bottom = values[2];
        }
        4 => {
            *top = values[0];
            *right = values[1];
            *bottom = values[2];
            *left = values[3];
        }
        _ => {}
    }
}

pub fn parse_border_style(kw: &str) -> BorderStyle {
    match kw {
        "none" => BorderStyle::None,
        "hidden" => BorderStyle::Hidden,
        "solid" => BorderStyle::Solid,
        "dashed" => BorderStyle::Dashed,
        "dotted" => BorderStyle::Dotted,
        "double" => BorderStyle::Double,
        "groove" => BorderStyle::Groove,
        "ridge" => BorderStyle::Ridge,
        "inset" => BorderStyle::Inset,
        "outset" => BorderStyle::Outset,
        _ => BorderStyle::None,
    }
}

fn parse_outline_style(kw: &str) -> Option<OutlineStyle> {
    match kw {
        "none" => Some(OutlineStyle::None),
        "hidden" => Some(OutlineStyle::Hidden),
        "solid" => Some(OutlineStyle::Solid),
        "dashed" => Some(OutlineStyle::Dashed),
        "dotted" => Some(OutlineStyle::Dotted),
        "double" => Some(OutlineStyle::Double),
        "groove" => Some(OutlineStyle::Groove),
        "ridge" => Some(OutlineStyle::Ridge),
        "inset" => Some(OutlineStyle::Inset),
        "outset" => Some(OutlineStyle::Outset),
        "auto" => Some(OutlineStyle::Auto),
        _ => None,
    }
}

fn parse_outline_width(value: &PropertyValue) -> Option<Length> {
    match value {
        PropertyValue::Length(l) if l.value >= 0.0 => Some(*l),
        PropertyValue::Keyword(kw) => match kw.as_str() {
            "thin" => Some(Length::px(1.0)),
            "medium" => Some(Length::px(3.0)),
            "thick" => Some(Length::px(5.0)),
            _ => None,
        },
        PropertyValue::Number(n) if *n >= 0.0 => Some(Length::px(*n)),
        _ => None,
    }
}

fn apply_outline_shorthand(styles: &mut ComputedStyle, value: &PropertyValue) {
    // The outline shorthand resets color/style/width to their initial values
    // before applying provided tokens (offset is not part of the shorthand).
    let defaults = ComputedStyle::default();
    styles.outline_style = defaults.outline_style;
    styles.outline_width = defaults.outline_width;
    styles.outline_color = defaults.outline_color;

    let mut color: Option<OutlineColor> = None;
    let mut style = None;
    let mut width = None;

    let tokens = match value {
        PropertyValue::Multiple(list) => list.clone(),
        other => vec![other.clone()],
    };

    for token in tokens {
        match token {
            PropertyValue::Color(c) => {
                color = Some(OutlineColor::Color(c));
            }
            PropertyValue::Keyword(ref kw) if kw.eq_ignore_ascii_case("currentcolor") => {
                color = Some(OutlineColor::CurrentColor);
            }
            PropertyValue::Keyword(ref kw) if kw.eq_ignore_ascii_case("invert") => {
                color = Some(OutlineColor::Invert);
            }
            PropertyValue::Keyword(ref kw) => {
                if let Some(parsed_style) = parse_outline_style(kw) {
                    style = Some(parsed_style);
                    continue;
                }
                match kw.as_str() {
                    "thin" | "medium" | "thick" => {
                        width = parse_outline_width(&PropertyValue::Keyword(kw.clone()));
                    }
                    _ => {}
                }
            }
            PropertyValue::Length(_) | PropertyValue::Number(_) => {
                if width.is_none() {
                    width = parse_outline_width(&token);
                }
            }
            _ => {}
        }
    }

    if let Some(c) = color {
        styles.outline_color = c;
    }
    if let Some(s) = style {
        styles.outline_style = s;
    }
    if let Some(w) = width {
        styles.outline_width = w;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::types::{
        AlignContent, AlignItems, AspectRatio, BackgroundRepeatKeyword, BoxSizing, CaseTransform, FontStretch,
        FontVariant, GridAutoFlow, GridTrack, ImageRendering, JustifyContent, ListStylePosition, ListStyleType,
        MixBlendMode, OutlineColor, OutlineStyle, PositionComponent, PositionKeyword, TextCombineUpright,
        TextDecorationLine, TextDecorationStyle, TextDecorationThickness, TextEmphasisFill, TextEmphasisPosition,
        TextEmphasisShape, TextEmphasisStyle, TextOrientation, TextTransform, WritingMode,
    };
    use cssparser::{Parser, ParserInput};

    #[test]
    fn parses_object_fit_keyword() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "object-fit".to_string(),
            value: PropertyValue::Keyword("cover".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.object_fit, ObjectFit::Cover);
    }

    #[test]
    fn parses_image_rendering_keywords() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "image-rendering".to_string(),
                value: PropertyValue::Keyword("pixelated".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.image_rendering, ImageRendering::Pixelated);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "image-rendering".to_string(),
                value: PropertyValue::Keyword("crisp-edges".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.image_rendering, ImageRendering::CrispEdges);
    }

    #[test]
    fn parses_containment_values() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "contain".to_string(),
                value: PropertyValue::Keyword("paint".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(style.containment.paint);
        assert!(!style.containment.layout);
        assert!(!style.containment.size);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "contain".to_string(),
                value: PropertyValue::Keyword("strict".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(style.containment.paint && style.containment.layout && style.containment.size);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "contain".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("layout".into()),
                    PropertyValue::Keyword("style".into()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(style.containment.layout && style.containment.style);
        assert!(!style.containment.paint);
    }

    #[test]
    fn parse_length_component_supports_all_units_case_insensitive() {
        let cases = [
            ("10PX", LengthUnit::Px),
            ("2rem", LengthUnit::Rem),
            ("3EM", LengthUnit::Em),
            ("1ex", LengthUnit::Ex),
            ("1ch", LengthUnit::Ch),
            ("5pt", LengthUnit::Pt),
            ("4pc", LengthUnit::Pc),
            ("2in", LengthUnit::In),
            ("1cm", LengthUnit::Cm),
            ("8mm", LengthUnit::Mm),
            ("6q", LengthUnit::Q),
            ("12vw", LengthUnit::Vw),
            ("14vh", LengthUnit::Vh),
            ("16vmin", LengthUnit::Vmin),
            ("18vmax", LengthUnit::Vmax),
        ];

        for (text, unit) in cases {
            let mut input = ParserInput::new(text);
            let mut parser = Parser::new(&mut input);
            let len = parse_length_component(&mut parser).expect("length");
            assert_eq!(len.unit, unit, "failed for {}", text);
        }
    }

    #[test]
    fn parse_length_component_handles_calc() {
        let mut input = ParserInput::new("calc(10px + 5px)");
        let mut parser = Parser::new(&mut input);
        let len = parse_length_component(&mut parser).expect("calc length");
        assert_eq!(len, Length::px(15.0));

        let mut input = ParserInput::new("calc(50% - 20%)");
        let mut parser = Parser::new(&mut input);
        let len = parse_length_component(&mut parser).expect("calc percent");
        assert_eq!(len, Length::percent(30.0));

        let mut input = ParserInput::new("min(2em, 3em)");
        let mut parser = Parser::new(&mut input);
        let len = parse_length_component(&mut parser).expect("min length");
        assert_eq!(len, Length::em(2.0));

        let mut input = ParserInput::new("clamp(10px, 12px, 15px)");
        let mut parser = Parser::new(&mut input);
        let len = parse_length_component(&mut parser).expect("clamp length");
        assert_eq!(len, Length::px(12.0));
    }

    #[test]
    fn parses_text_orientation_keywords() {
        let mut styles = ComputedStyle::default();
        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-orientation".into(),
                value: PropertyValue::Keyword("upright".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(styles.text_orientation, TextOrientation::Upright);

        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-orientation".into(),
                value: PropertyValue::Keyword("sideways-right".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(styles.text_orientation, TextOrientation::SidewaysRight);
    }

    #[test]
    fn leaves_text_orientation_unchanged_on_unknown_keyword() {
        let mut styles = ComputedStyle::default();
        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-orientation".into(),
                value: PropertyValue::Keyword("upright".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-orientation".into(),
                value: PropertyValue::Keyword("invalid".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(styles.text_orientation, TextOrientation::Upright);
    }

    #[test]
    fn parses_text_combine_upright_values() {
        let mut styles = ComputedStyle::default();
        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-combine-upright".into(),
                value: PropertyValue::Keyword("digits".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(styles.text_combine_upright, TextCombineUpright::Digits(2));

        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-combine-upright".into(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("digits".into()),
                    PropertyValue::Number(3.0),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(styles.text_combine_upright, TextCombineUpright::Digits(3));

        apply_declaration(
            &mut styles,
            &Declaration {
                property: "text-combine-upright".into(),
                value: PropertyValue::Keyword("all".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(styles.text_combine_upright, TextCombineUpright::All);
    }

    #[test]
    fn parses_text_transform_keyword() {
        let mut styles = ComputedStyle::default();
        let prop = Declaration {
            property: "text-transform".into(),
            value: PropertyValue::Keyword("uppercase".into()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut styles, &prop, 16.0, 16.0);
        assert_eq!(
            styles.text_transform,
            TextTransform::with_case(CaseTransform::Uppercase)
        );
    }

    #[test]
    fn parses_aspect_ratio_keywords_and_numbers() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "aspect-ratio".to_string(),
                value: PropertyValue::Keyword("16/9".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(matches!(style.aspect_ratio, AspectRatio::Ratio(r) if (r - (16.0/9.0)).abs() < 0.0001));

        apply_declaration(
            &mut style,
            &Declaration {
                property: "aspect-ratio".to_string(),
                value: PropertyValue::Number(2.0),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(matches!(style.aspect_ratio, AspectRatio::Ratio(r) if (r - 2.0).abs() < 0.0001));

        apply_declaration(
            &mut style,
            &Declaration {
                property: "aspect-ratio".to_string(),
                value: PropertyValue::Keyword("auto".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(matches!(style.aspect_ratio, AspectRatio::Auto));
    }

    #[test]
    fn parses_alignment_keywords_with_start_variants() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "align-items".to_string(),
                value: PropertyValue::Keyword("start".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.align_items, AlignItems::Start);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "align-self".to_string(),
                value: PropertyValue::Keyword("self-end".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.align_self, Some(AlignItems::SelfEnd));

        apply_declaration(
            &mut style,
            &Declaration {
                property: "justify-items".to_string(),
                value: PropertyValue::Keyword("right".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.justify_items, AlignItems::End);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "justify-self".to_string(),
                value: PropertyValue::Keyword("auto".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(style.justify_self.is_none());

        apply_declaration(
            &mut style,
            &Declaration {
                property: "justify-self".to_string(),
                value: PropertyValue::Keyword("center".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.justify_self, Some(AlignItems::Center));
    }

    #[test]
    fn parses_place_shorthands() {
        let mut style = ComputedStyle::default();

        apply_declaration(
            &mut style,
            &Declaration {
                property: "place-items".to_string(),
                value: PropertyValue::Keyword("center stretch".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.align_items, AlignItems::Center);
        assert_eq!(style.justify_items, AlignItems::Stretch);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "place-self".to_string(),
                value: PropertyValue::Keyword("end start".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.align_self, Some(AlignItems::End));
        assert_eq!(style.justify_self, Some(AlignItems::Start));

        apply_declaration(
            &mut style,
            &Declaration {
                property: "place-content".to_string(),
                value: PropertyValue::Keyword("space-between center".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.align_content, AlignContent::SpaceBetween);
        assert_eq!(style.justify_content, JustifyContent::Center);
    }

    #[test]
    fn parses_writing_mode_keywords() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "writing-mode".to_string(),
                value: PropertyValue::Keyword("vertical-rl".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.writing_mode, WritingMode::VerticalRl);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "writing-mode".to_string(),
                value: PropertyValue::Keyword("sideways-lr".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.writing_mode, WritingMode::SidewaysLr);
    }

    #[test]
    fn parses_object_position_keywords() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "object-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("right".to_string()),
                PropertyValue::Keyword("bottom".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.object_position.x,
            PositionComponent::Keyword(PositionKeyword::End)
        ));
        assert!(matches!(
            style.object_position.y,
            PositionComponent::Keyword(PositionKeyword::End)
        ));
    }

    #[test]
    fn parses_box_sizing_keyword() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "box-sizing".to_string(),
                value: PropertyValue::Keyword("border-box".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert!(matches!(style.box_sizing, BoxSizing::BorderBox));
    }

    #[test]
    fn parses_grid_auto_rows_and_columns() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "grid-auto-rows".into(),
                value: PropertyValue::Keyword("10px minmax(0,1fr)".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_auto_rows.len(), 2);
        assert!(matches!(style.grid_auto_rows[0], GridTrack::Length(_)));
        apply_declaration(
            &mut style,
            &Declaration {
                property: "grid-auto-columns".into(),
                value: PropertyValue::Keyword("20%".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_auto_columns.len(), 1);
    }

    #[test]
    fn parses_grid_auto_flow() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "grid-auto-flow".into(),
                value: PropertyValue::Keyword("column dense".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_auto_flow, GridAutoFlow::ColumnDense);
    }

    #[test]
    fn parses_grid_area_named_area() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "grid-area".into(),
                value: PropertyValue::Keyword("hero".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_row_raw.as_deref(), Some("hero-start / hero-end"));
        assert_eq!(style.grid_column_raw.as_deref(), Some("hero-start / hero-end"));
    }

    #[test]
    fn parses_grid_shorthand_auto_flow_form() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "grid".into(),
                value: PropertyValue::Keyword("auto-flow 10px / 20px".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_auto_flow, GridAutoFlow::Row);
        assert_eq!(style.grid_auto_rows.len(), 1);
        assert_eq!(style.grid_auto_columns.len(), 1);
    }

    #[test]
    fn parses_grid_shorthand_template_form() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "grid".into(),
                value: PropertyValue::Keyword("\"a a\" \"b b\" / 1fr 2fr".into()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_template_rows.len(), 2);
        assert_eq!(style.grid_template_columns.len(), 2);
        assert_eq!(style.grid_template_areas.len(), 2);
    }

    #[test]
    fn gap_shorthand_parses_two_values_and_percent() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "gap".to_string(),
                value: PropertyValue::Keyword("10px 20%".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.grid_row_gap, Length::px(10.0));
        assert_eq!(style.grid_column_gap, Length::percent(20.0));
    }

    #[test]
    fn row_and_column_gap_accept_percent_and_normal() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "row-gap".to_string(),
                value: PropertyValue::Keyword("normal".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.grid_row_gap, Length::px(0.0));

        let mut style2 = ComputedStyle::default();
        apply_declaration(
            &mut style2,
            &Declaration {
                property: "column-gap".to_string(),
                value: PropertyValue::Percentage(15.0),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style2.grid_column_gap, Length::percent(15.0));
    }

    #[test]
    fn outline_shorthand_resets_missing_parts() {
        let mut style = ComputedStyle::default();
        style.outline_style = OutlineStyle::Solid;
        style.outline_width = Length::px(8.0);
        style.outline_color = OutlineColor::Color(Rgba::GREEN);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "outline".to_string(),
                value: PropertyValue::Color(Rgba::RED),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.outline_style, OutlineStyle::None);
        assert_eq!(style.outline_width, Length::px(3.0)); // medium
        assert_eq!(style.outline_color, OutlineColor::Color(Rgba::RED));
    }

    #[test]
    fn outline_shorthand_defaults_color_to_currentcolor() {
        let mut style = ComputedStyle::default();
        style.color = Rgba::BLUE;
        style.outline_color = OutlineColor::Color(Rgba::GREEN);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "outline".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("solid".to_string()),
                    PropertyValue::Keyword("thin".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.outline_style, OutlineStyle::Solid);
        assert_eq!(style.outline_width, Length::px(1.0));
        assert_eq!(style.outline_color, OutlineColor::Invert);
        // currentColor resolution happens at paint time; initial value is invert per spec
    }

    #[test]
    fn negative_outline_width_is_ignored() {
        let mut style = ComputedStyle::default();
        style.outline_width = Length::px(5.0);
        apply_declaration(
            &mut style,
            &Declaration {
                property: "outline-width".to_string(),
                value: PropertyValue::Number(-3.0),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.outline_width, Length::px(5.0));
    }

    #[test]
    fn overflow_clip_sets_both_axes() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "overflow".to_string(),
                value: PropertyValue::Keyword("clip".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.overflow_x, Overflow::Clip);
        assert_eq!(style.overflow_y, Overflow::Clip);
    }

    #[test]
    fn parses_background_position_keywords() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("left".to_string()),
                PropertyValue::Keyword("top".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert!((y.alignment - 0.0).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!(y.offset.is_zero());
    }

    #[test]
    fn background_position_two_value_second_length_is_vertical() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("left".to_string()),
                    PropertyValue::Percentage(20.0),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!((y.alignment - 0.0).abs() < 0.01);
        assert_eq!(y.offset, Length::percent(20.0));
    }

    #[test]
    fn background_position_two_keywords_swapped_axes() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("top".to_string()),
                    PropertyValue::Keyword("right".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!((y.alignment - 0.0).abs() < 0.01);
    }

    #[test]
    fn background_position_center_left_swaps_axes() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("center".to_string()),
                    PropertyValue::Keyword("left".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert!((y.alignment - 0.5).abs() < 0.01);
    }

    #[test]
    fn background_position_center_bottom_keeps_vertical() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("center".to_string()),
                    PropertyValue::Keyword("bottom".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.5).abs() < 0.01);
        assert!((y.alignment - 1.0).abs() < 0.01);
    }

    #[test]
    fn background_position_three_values_horizontal_pair() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("left".to_string()),
                    PropertyValue::Length(Length::px(10.0)),
                    PropertyValue::Keyword("bottom".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert_eq!(x.offset, Length::px(10.0));
        assert!((y.alignment - 1.0).abs() < 0.01);
        assert!(y.offset.is_zero());
    }

    #[test]
    fn background_position_three_values_vertical_pair() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("bottom".to_string()),
                    PropertyValue::Length(Length::px(5.0)),
                    PropertyValue::Keyword("right".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((y.alignment - 1.0).abs() < 0.01);
        assert_eq!(y.offset, Length::px(-5.0));
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!(x.offset.is_zero());
    }

    #[test]
    fn background_position_four_values() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("right".to_string()),
                    PropertyValue::Length(Length::px(20.0)),
                    PropertyValue::Keyword("bottom".to_string()),
                    PropertyValue::Length(Length::px(10.0)),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert_eq!(x.offset, Length::px(-20.0));
        assert!((y.alignment - 1.0).abs() < 0.01);
        assert_eq!(y.offset, Length::px(-10.0));
    }

    #[test]
    fn background_position_four_values_vertical_first() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("top".to_string()),
                    PropertyValue::Length(Length::px(10.0)),
                    PropertyValue::Keyword("left".to_string()),
                    PropertyValue::Length(Length::px(5.0)),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((y.alignment - 0.0).abs() < 0.01);
        assert_eq!(y.offset, Length::px(10.0));
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert_eq!(x.offset, Length::px(5.0));
    }

    #[test]
    fn background_position_x_and_y_longhands_merge_layers() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Url("a.png".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Url("b.png".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position-y".to_string(),
                value: PropertyValue::Keyword("bottom".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position-x".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("left".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Percentage(20.0),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        let BackgroundPosition::Position { x: x0, y: y0 } = style.background_layers[0].position;
        assert!((x0.alignment - 0.0).abs() < 0.01);
        assert!(x0.offset.is_zero());
        assert!((y0.alignment - 1.0).abs() < 0.01);

        let BackgroundPosition::Position { x: x1, y: y1 } = style.background_layers[1].position;
        assert_eq!(x1.offset, Length::percent(20.0));
        assert!((x1.alignment - 0.0).abs() < 0.01);
        assert!((y1.alignment - 1.0).abs() < 0.01);
    }

    #[test]
    fn background_position_y_longhand_repeats_for_layers() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Url("a.png".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Url("b.png".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position-y".to_string(),
                value: PropertyValue::Percentage(10.0),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        let BackgroundPosition::Position { y: y0, .. } = style.background_layers[0].position;
        let BackgroundPosition::Position { y: y1, .. } = style.background_layers[1].position;
        assert_eq!(y0.offset, Length::percent(10.0));
        assert_eq!(y1.offset, Length::percent(10.0));
    }

    #[test]
    fn parses_background_position_end_offsets() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("right".to_string()),
                PropertyValue::Percentage(10.0),
                PropertyValue::Keyword("bottom".to_string()),
                PropertyValue::Length(Length::px(5.0)),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!((y.alignment - 1.0).abs() < 0.01);
        assert!((x.offset.value + 10.0).abs() < 0.01 && x.offset.unit == LengthUnit::Percent);
        assert!((y.offset.value + 5.0).abs() < 0.01 && y.offset.unit == LengthUnit::Px);
    }

    #[test]
    fn parses_background_position_with_offsets_and_axis_defaults() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("right".to_string()),
                PropertyValue::Length(Length::px(10.0)),
                PropertyValue::Keyword("top".to_string()),
                PropertyValue::Percentage(25.0),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!((y.alignment - 0.0).abs() < 0.01);
        assert!((x.offset.value + 10.0).abs() < 0.01 && x.offset.unit == LengthUnit::Px);
        assert!((y.offset.value - 25.0).abs() < 0.01 && y.offset.unit == LengthUnit::Percent);

        // Default the missing axis to center when only one axis is provided.
        let decl = Declaration {
            property: "background-position".to_string(),
            value: PropertyValue::Keyword("top".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.5).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!((y.alignment - 0.0).abs() < 0.01);
    }

    #[test]
    fn parses_text_decoration_longhands() {
        let mut style = ComputedStyle::default();

        let decl = Declaration {
            property: "text-decoration-line".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("underline".to_string()),
                PropertyValue::Keyword("overline".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.text_decoration.lines.contains(TextDecorationLine::UNDERLINE));
        assert!(style.text_decoration.lines.contains(TextDecorationLine::OVERLINE));

        let decl = Declaration {
            property: "text-decoration-style".to_string(),
            value: PropertyValue::Keyword("dashed".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.text_decoration.style, TextDecorationStyle::Dashed);

        let decl = Declaration {
            property: "text-decoration-color".to_string(),
            value: PropertyValue::Color(Rgba::BLUE),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.text_decoration.color, Some(Rgba::BLUE));

        let decl = Declaration {
            property: "text-decoration-thickness".to_string(),
            value: PropertyValue::Length(Length::px(3.0)),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.text_decoration.thickness,
            TextDecorationThickness::Length(l) if (l.to_px() - 3.0).abs() < 0.01
        ));

        let decl = Declaration {
            property: "text-decoration-thickness".to_string(),
            value: PropertyValue::Keyword("from-font".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.text_decoration.thickness,
            TextDecorationThickness::FromFont
        ));

        let decl = Declaration {
            property: "text-underline-position".to_string(),
            value: PropertyValue::Keyword("under".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.text_underline_position, TextUnderlinePosition::Under));

        let decl = Declaration {
            property: "text-underline-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("left".to_string()),
                PropertyValue::Keyword("under".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.text_underline_position,
            TextUnderlinePosition::UnderLeft
        ));
    }

    #[test]
    fn parses_text_emphasis_properties() {
        let mut style = ComputedStyle::default();

        let decl = Declaration {
            property: "text-emphasis-style".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("open".to_string()),
                PropertyValue::Keyword("sesame".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.text_emphasis_style,
            TextEmphasisStyle::Mark {
                fill: TextEmphasisFill::Open,
                shape: TextEmphasisShape::Sesame
            }
        ));

        let decl = Declaration {
            property: "text-emphasis-color".to_string(),
            value: PropertyValue::Color(Rgba::RED),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.text_emphasis_color, Some(Rgba::RED));

        let decl = Declaration {
            property: "text-emphasis-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("under".to_string()),
                PropertyValue::Keyword("right".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.text_emphasis_position, TextEmphasisPosition::UnderRight));

        let decl = Declaration {
            property: "text-emphasis".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("circle".to_string()),
                PropertyValue::Color(Rgba::BLUE),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.text_emphasis_style,
            TextEmphasisStyle::Mark {
                fill: TextEmphasisFill::Filled,
                shape: TextEmphasisShape::Circle
            }
        ));
        assert_eq!(style.text_emphasis_color, Some(Rgba::BLUE));
    }

    #[test]
    fn text_underline_position_rejects_invalid_combinations() {
        let mut style = ComputedStyle::default();
        style.text_underline_position = TextUnderlinePosition::Under;

        let decl = Declaration {
            property: "text-underline-position".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("auto".to_string()),
                PropertyValue::Keyword("under".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert!(
            matches!(style.text_underline_position, TextUnderlinePosition::Under),
            "invalid keyword combinations should be ignored without changing the computed value"
        );
    }

    #[test]
    fn parses_text_decoration_shorthand_and_resets() {
        let mut style = ComputedStyle::default();
        style.text_decoration.lines = TextDecorationLine::LINE_THROUGH;
        style.text_decoration.style = TextDecorationStyle::Double;
        style.text_decoration.color = Some(Rgba::BLUE);
        style.text_decoration.thickness = TextDecorationThickness::Length(Length::px(2.0));

        let decl = Declaration {
            property: "text-decoration".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("underline".to_string()),
                PropertyValue::Keyword("dotted".to_string()),
                PropertyValue::Color(Rgba::RED),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert!(style.text_decoration.lines.contains(TextDecorationLine::UNDERLINE));
        assert!(!style.text_decoration.lines.contains(TextDecorationLine::LINE_THROUGH));
        assert_eq!(style.text_decoration.style, TextDecorationStyle::Dotted);
        assert_eq!(style.text_decoration.color, Some(Rgba::RED));
        assert!(matches!(style.text_decoration.thickness, TextDecorationThickness::Auto));

        // currentcolor leaves color unset, shorthand resets missing pieces back to initial
        let decl = Declaration {
            property: "text-decoration".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("line-through".to_string()),
                PropertyValue::Keyword("wavy".to_string()),
                PropertyValue::Keyword("currentcolor".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.text_decoration.lines.contains(TextDecorationLine::LINE_THROUGH));
        assert_eq!(style.text_decoration.style, TextDecorationStyle::Wavy);
        assert_eq!(style.text_decoration.color, None);
        assert!(matches!(style.text_decoration.thickness, TextDecorationThickness::Auto));

        // thickness in shorthand
        let decl = Declaration {
            property: "text-decoration".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("overline".to_string()),
                PropertyValue::Keyword("double".to_string()),
                PropertyValue::Color(Rgba::GREEN),
                PropertyValue::Length(Length::px(3.2)),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.text_decoration.lines.contains(TextDecorationLine::OVERLINE));
        assert_eq!(style.text_decoration.style, TextDecorationStyle::Double);
        assert_eq!(style.text_decoration.color, Some(Rgba::GREEN));
        assert!(matches!(
            style.text_decoration.thickness,
            TextDecorationThickness::Length(l) if (l.to_px() - 3.2).abs() < 0.01
        ));
    }

    #[test]
    fn parses_list_style_properties() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "list-style-type".to_string(),
            value: PropertyValue::Keyword("square".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::Square);

        let decl = Declaration {
            property: "list-style-position".to_string(),
            value: PropertyValue::Keyword("inside".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_position, ListStylePosition::Inside);

        let decl = Declaration {
            property: "list-style-image".to_string(),
            value: PropertyValue::Url("marker.png".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_image, ListStyleImage::Url("marker.png".to_string()));

        let decl = Declaration {
            property: "list-style".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("upper-roman".to_string()),
                PropertyValue::Keyword("outside".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::UpperRoman);
        assert_eq!(style.list_style_position, ListStylePosition::Outside);
        assert_eq!(style.list_style_image, ListStyleImage::None);

        let decl = Declaration {
            property: "list-style".to_string(),
            value: PropertyValue::Url("img.png".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_image, ListStyleImage::Url("img.png".to_string()));
        assert_eq!(style.list_style_type, ListStyleType::Disc);
        assert_eq!(style.list_style_position, ListStylePosition::Outside);

        let decl = Declaration {
            property: "list-style".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::None);
        assert_eq!(style.list_style_image, ListStyleImage::None);

        let decl = Declaration {
            property: "list-style-type".to_string(),
            value: PropertyValue::Keyword("lower-greek".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::LowerGreek);

        let decl = Declaration {
            property: "list-style-type".to_string(),
            value: PropertyValue::Keyword("armenian".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::Armenian);

        let decl = Declaration {
            property: "list-style-type".to_string(),
            value: PropertyValue::Keyword("lower-armenian".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::LowerArmenian);

        let decl = Declaration {
            property: "list-style-type".to_string(),
            value: PropertyValue::Keyword("georgian".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::Georgian);
    }

    #[test]
    fn cursor_keyword_parses() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "cursor".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("pointer".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.cursor, CursorKeyword::Pointer);
        assert!(style.cursor_images.is_empty());
    }

    #[test]
    fn cursor_allows_custom_image_and_hotspot_with_fallback() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "cursor".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Url("cursor.cur".to_string()),
                    PropertyValue::Number(5.0),
                    PropertyValue::Number(7.0),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Keyword("move".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.cursor, CursorKeyword::Move);
        assert_eq!(style.cursor_images.len(), 1);
        assert_eq!(style.cursor_images[0].url, "cursor.cur");
        assert_eq!(style.cursor_images[0].hotspot, Some((5.0, 7.0)));
    }

    #[test]
    fn cursor_accepts_image_set_and_fallback_keyword() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "cursor".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("image-set(url(\"c1.cur\") 1x, url(\"c2.cur\") 2x)".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Keyword("crosshair".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.cursor, CursorKeyword::Crosshair);
        assert_eq!(style.cursor_images.len(), 1);
        assert_eq!(style.cursor_images[0].url, "c1.cur");
    }

    #[test]
    fn list_style_image_accepts_image_set() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "list-style-image".to_string(),
                value: PropertyValue::Keyword(
                    "image-set(url(\"marker-1x.png\") 1x, url(\"marker-2x.png\") 2x)".to_string(),
                ),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        assert_eq!(style.list_style_image, ListStyleImage::Url("marker-1x.png".to_string()));
    }

    #[test]
    fn parses_quotes_property() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "quotes".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::String("«".to_string()),
                PropertyValue::String("»".to_string()),
                PropertyValue::String("‹".to_string()),
                PropertyValue::String("›".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.quotes,
            vec![("«".to_string(), "»".to_string()), ("‹".to_string(), "›".to_string())]
        );

        let decl = Declaration {
            property: "quotes".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.quotes.is_empty());
    }

    #[test]
    fn parses_letter_and_word_spacing() {
        let mut style = ComputedStyle::default();
        style.font_size = 20.0;
        style.letter_spacing = 3.0;

        let decl = Declaration {
            property: "letter-spacing".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.letter_spacing, 0.0);

        let decl = Declaration {
            property: "letter-spacing".to_string(),
            value: PropertyValue::Length(Length::em(0.25)),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.letter_spacing - 5.0).abs() < 0.01);

        let decl = Declaration {
            property: "word-spacing".to_string(),
            value: PropertyValue::Percentage(50.0),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.word_spacing - 10.0).abs() < 0.01);

        let decl = Declaration {
            property: "word-spacing".to_string(),
            value: PropertyValue::Length(Length::em(-0.5)),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.word_spacing + 10.0).abs() < 0.01);
    }

    #[test]
    fn parses_counter_properties() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "counter-reset".to_string(),
            value: PropertyValue::Keyword("chapter 3 section".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let reset = style.counters.counter_reset.as_ref().unwrap();
        assert_eq!(reset.items.len(), 2);
        assert_eq!(reset.items[0].name, "chapter");
        assert_eq!(reset.items[0].value, 3);
        assert_eq!(reset.items[1].name, "section");
        assert_eq!(reset.items[1].value, 0);

        let decl = Declaration {
            property: "counter-increment".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("item".to_string()),
                PropertyValue::Number(2.0),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let increment = style.counters.counter_increment.as_ref().unwrap();
        assert_eq!(increment.items.len(), 1);
        assert_eq!(increment.items[0].name, "item");
        assert_eq!(increment.items[0].value, 2);

        let decl = Declaration {
            property: "counter-set".to_string(),
            value: PropertyValue::Keyword("item 7".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let set = style.counters.counter_set.as_ref().unwrap();
        assert_eq!(set.items.len(), 1);
        assert_eq!(set.items[0].name, "item");
        assert_eq!(set.items[0].value, 7);

        let decl = Declaration {
            property: "counter-increment".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style
            .counters
            .counter_increment
            .as_ref()
            .map(|c| c.is_empty())
            .unwrap_or(false));
    }

    #[test]
    fn background_shorthand_resets_unspecified_fields() {
        let mut style = ComputedStyle::default();
        style.set_background_layers(vec![BackgroundLayer {
            repeat: BackgroundRepeat::repeat_x(),
            attachment: BackgroundAttachment::Fixed,
            position: BackgroundPosition::Position {
                x: crate::style::types::BackgroundPositionComponent {
                    alignment: 1.0,
                    offset: Length::px(5.0),
                },
                y: crate::style::types::BackgroundPositionComponent {
                    alignment: 1.0,
                    offset: Length::px(5.0),
                },
            },
            size: BackgroundSize::Explicit(
                BackgroundSizeComponent::Length(Length::px(10.0)),
                BackgroundSizeComponent::Auto,
            ),
            origin: BackgroundBox::ContentBox,
            clip: BackgroundBox::ContentBox,
            ..BackgroundLayer::default()
        }]);

        let decl = Declaration {
            property: "background".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Url("example.png".to_string()),
                PropertyValue::Keyword("no-repeat".to_string()),
                PropertyValue::Keyword("right".to_string()),
                PropertyValue::Keyword("/".to_string()),
                PropertyValue::Keyword("contain".to_string()),
                PropertyValue::Keyword("fixed".to_string()),
                PropertyValue::Keyword("content-box".to_string()),
                PropertyValue::Keyword("padding-box".to_string()),
                PropertyValue::Color(Rgba::RED),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert_eq!(style.background_color, Rgba::RED);
        let layer = &style.background_layers[0];
        assert!(matches!(layer.image, Some(BackgroundImage::Url(ref s)) if s == "example.png"));
        assert_eq!(layer.repeat, BackgroundRepeat::no_repeat());
        let BackgroundPosition::Position { x, y } = layer.position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!((y.alignment - 0.5).abs() < 0.01);
        assert!(y.offset.is_zero());
        assert_eq!(layer.size, BackgroundSize::Keyword(BackgroundSizeKeyword::Contain));
        assert_eq!(layer.attachment, BackgroundAttachment::Fixed);
        assert_eq!(layer.origin, BackgroundBox::ContentBox);
        assert_eq!(layer.clip, BackgroundBox::PaddingBox);
    }

    #[test]
    fn background_longhand_lists_expand_layers() {
        let mut style = ComputedStyle::default();
        // Two images
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Url("a.png".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Url("b.png".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.background_layers.len(), 2);
        // Only one size provided; values repeat across layers
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-size".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Length(Length::px(10.0)),
                    PropertyValue::Length(Length::px(20.0)),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(
            style.background_layers[0].size,
            BackgroundSize::Explicit(
                BackgroundSizeComponent::Length(Length::px(10.0)),
                BackgroundSizeComponent::Length(Length::px(20.0))
            )
        );
        assert_eq!(
            style.background_layers[1].size,
            BackgroundSize::Explicit(
                BackgroundSizeComponent::Length(Length::px(10.0)),
                BackgroundSizeComponent::Length(Length::px(20.0))
            )
        );
        // Two repeats; both layers updated
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-repeat".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("no-repeat".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Keyword("space".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.background_layers[0].repeat, BackgroundRepeat::no_repeat());
        assert_eq!(style.background_layers[1].repeat.x, BackgroundRepeatKeyword::Space);
    }

    #[test]
    fn background_longhand_extra_values_drop_with_single_image_layer() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Url("one.png".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        // Two positions, but only one image -> second is ignored for layer construction
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-position".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("left".to_string()),
                    PropertyValue::Keyword("top".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Keyword("right".to_string()),
                    PropertyValue::Keyword("bottom".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.background_layers.len(), 1);
        let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert!((y.alignment - 0.0).abs() < 0.01);
    }

    #[test]
    fn background_image_image_set_picks_1x_candidate() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Keyword("image-set(url(\"low.png\") 1x, url(\"retina.png\") 2x)".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        let layer = &style.background_layers[0];
        assert!(matches!(
            layer.image,
            Some(BackgroundImage::Url(ref url)) if url == "low.png"
        ));
    }

    #[test]
    fn background_image_image_set_honors_device_pixel_ratio() {
        let mut style = ComputedStyle::default();
        with_image_set_dpr(2.0, || {
            apply_declaration(
                &mut style,
                &Declaration {
                    property: "background-image".to_string(),
                    value: PropertyValue::Keyword("image-set(url(\"low.png\") 1x, url(\"retina.png\") 2x)".to_string()),
                    raw_value: String::new(),
                    important: false,
                },
                16.0,
                16.0,
            );
        });

        assert!(matches!(
            style.background_layers[0].image,
            Some(BackgroundImage::Url(ref url)) if url == "retina.png"
        ));
    }

    #[test]
    fn background_image_image_set_uses_best_available_resolution() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Keyword("image-set(url(\"hi.png\") 192dpi)".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(matches!(
            style.background_layers[0].image,
            Some(BackgroundImage::Url(ref url)) if url == "hi.png"
        ));

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Keyword(
                    "image-set(url(\"smaller.png\") 0.5x, url(\"bigger.png\") 0.75x)".to_string(),
                ),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert!(matches!(
            style.background_layers[0].image,
            Some(BackgroundImage::Url(ref url)) if url == "bigger.png"
        ));
    }

    #[test]
    fn background_shorthand_accepts_image_set() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("image-set(url(\"one-x.png\") 1x, url(\"two-x.png\") 2x)".to_string()),
                    PropertyValue::Keyword("no-repeat".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        let layer = &style.background_layers[0];
        assert!(matches!(
            layer.image,
            Some(BackgroundImage::Url(ref url)) if url == "one-x.png"
        ));
        assert_eq!(layer.repeat, BackgroundRepeat::no_repeat());
    }

    #[test]
    fn background_blend_mode_repeats_and_truncates() {
        let mut style = ComputedStyle::default();
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Url("a.png".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Url("b.png".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-blend-mode".to_string(),
                value: PropertyValue::Keyword("screen".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.background_layers[0].blend_mode, MixBlendMode::Screen);
        assert_eq!(style.background_layers[1].blend_mode, MixBlendMode::Screen);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-blend-mode".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("multiply".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Keyword("overlay".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.background_layers[0].blend_mode, MixBlendMode::Multiply);
        assert_eq!(style.background_layers[1].blend_mode, MixBlendMode::Overlay);

        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-image".to_string(),
                value: PropertyValue::Url("single.png".to_string()),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        apply_declaration(
            &mut style,
            &Declaration {
                property: "background-blend-mode".to_string(),
                value: PropertyValue::Multiple(vec![
                    PropertyValue::Keyword("darken".to_string()),
                    PropertyValue::Keyword(",".to_string()),
                    PropertyValue::Keyword("lighten".to_string()),
                ]),
                raw_value: String::new(),
                important: false,
            },
            16.0,
            16.0,
        );
        assert_eq!(style.background_layers.len(), 1);
        assert_eq!(style.background_layers[0].blend_mode, MixBlendMode::Darken);
    }

    #[test]
    fn background_shorthand_color_only_resets_to_initials() {
        let mut style = ComputedStyle::default();
        style.set_background_layers(vec![BackgroundLayer {
            repeat: BackgroundRepeat::repeat_x(),
            origin: BackgroundBox::ContentBox,
            clip: BackgroundBox::ContentBox,
            size: BackgroundSize::Explicit(
                BackgroundSizeComponent::Length(Length::px(12.0)),
                BackgroundSizeComponent::Auto,
            ),
            ..BackgroundLayer::default()
        }]);

        let decl = Declaration {
            property: "background".to_string(),
            value: PropertyValue::Color(Rgba::RED),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert_eq!(style.background_color, Rgba::RED);
        assert!(style.background_layers.first().and_then(|l| l.image.as_ref()).is_none());
        let layer = &style.background_layers[0];
        assert_eq!(layer.repeat, BackgroundRepeat::repeat());
        let BackgroundPosition::Position { x, y } = layer.position;
        assert!(x.offset.is_zero() && (x.alignment - 0.0).abs() < 0.01);
        assert!(y.offset.is_zero() && (y.alignment - 0.0).abs() < 0.01);
        assert_eq!(
            layer.size,
            BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
        );
        assert_eq!(layer.attachment, BackgroundAttachment::Scroll);
        assert_eq!(layer.origin, BackgroundBox::PaddingBox);
        assert_eq!(layer.clip, BackgroundBox::BorderBox);
    }

    #[test]
    fn background_shorthand_none_resets_and_clears_image() {
        let mut style = ComputedStyle::default();
        style.background_color = Rgba::RED;
        style.set_background_layers(vec![BackgroundLayer {
            image: Some(BackgroundImage::Url("foo.png".to_string())),
            repeat: BackgroundRepeat::repeat_x(),
            origin: BackgroundBox::ContentBox,
            ..BackgroundLayer::default()
        }]);

        let decl = Declaration {
            property: "background".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert_eq!(style.background_color, Rgba::TRANSPARENT);
        assert_eq!(style.background_layers.len(), 1);
        let layer = &style.background_layers[0];
        assert!(layer.image.is_none());
        assert_eq!(layer.repeat, BackgroundRepeat::repeat());
        assert_eq!(layer.origin, BackgroundBox::PaddingBox);
        assert_eq!(layer.clip, BackgroundBox::BorderBox);
    }

    #[test]
    fn parses_background_repeat_keywords_and_pairs() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-repeat".to_string(),
            value: PropertyValue::Keyword("space".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.background_layers[0].repeat.x, BackgroundRepeatKeyword::Space);
        assert_eq!(style.background_layers[0].repeat.y, BackgroundRepeatKeyword::Space);

        let decl = Declaration {
            property: "background-repeat".to_string(),
            value: PropertyValue::Keyword("repeat-x".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.background_layers[0].repeat.x, BackgroundRepeatKeyword::Repeat);
        assert_eq!(style.background_layers[0].repeat.y, BackgroundRepeatKeyword::NoRepeat);

        let decl = Declaration {
            property: "background-repeat".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("space".to_string()),
                PropertyValue::Keyword("round".to_string()),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.background_layers[0].repeat.x, BackgroundRepeatKeyword::Space);
        assert_eq!(style.background_layers[0].repeat.y, BackgroundRepeatKeyword::Round);
    }

    #[test]
    fn parses_background_size_components_and_defaults() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-size".to_string(),
            value: PropertyValue::Length(Length::px(25.0)),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.background_layers[0].size,
            BackgroundSize::Explicit(
                BackgroundSizeComponent::Length(Length::px(25.0)),
                BackgroundSizeComponent::Auto
            )
        );

        let decl = Declaration {
            property: "background-size".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("auto".to_string()),
                PropertyValue::Percentage(50.0),
            ]),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.background_layers[0].size,
            BackgroundSize::Explicit(
                BackgroundSizeComponent::Auto,
                BackgroundSizeComponent::Length(Length::percent(50.0))
            )
        );

        let decl = Declaration {
            property: "background-size".to_string(),
            value: PropertyValue::Keyword("contain".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.background_layers[0].size,
            BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)
        );
    }

    #[test]
    fn parses_white_space_break_spaces() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "white-space".to_string(),
            value: PropertyValue::Keyword("break-spaces".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.white_space, WhiteSpace::BreakSpaces);
    }

    #[test]
    fn parses_line_break_anywhere() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "line-break".to_string(),
            value: PropertyValue::Keyword("anywhere".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.line_break, LineBreak::Anywhere);
    }

    #[test]
    fn parses_background_origin_and_clip() {
        let mut style = ComputedStyle::default();

        let origin_decl = Declaration {
            property: "background-origin".to_string(),
            value: PropertyValue::Keyword("content-box".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &origin_decl, 16.0, 16.0);
        assert_eq!(style.background_layers[0].origin, BackgroundBox::ContentBox);

        let clip_decl = Declaration {
            property: "background-clip".to_string(),
            value: PropertyValue::Keyword("padding-box".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &clip_decl, 16.0, 16.0);
        assert_eq!(style.background_layers[0].clip, BackgroundBox::PaddingBox);
    }

    #[test]
    fn parses_filter_list_with_lengths_and_numbers() {
        let filters =
            parse_filter_list(&PropertyValue::Keyword("blur(4px) brightness(50%)".to_string())).expect("filters");
        assert_eq!(filters.len(), 2);
        match &filters[0] {
            FilterFunction::Blur(len) => assert!((len.to_px() - 4.0).abs() < 0.01),
            _ => panic!("expected blur filter"),
        }
        match &filters[1] {
            FilterFunction::Brightness(v) => assert!((*v - 0.5).abs() < 0.001),
            _ => panic!("expected brightness filter"),
        }
    }

    #[test]
    fn parses_drop_shadow_defaulting_to_current_color() {
        let filters =
            parse_filter_list(&PropertyValue::Keyword("drop-shadow(2px 3px 4px)".to_string())).expect("filters");
        assert_eq!(filters.len(), 1);
        match &filters[0] {
            FilterFunction::DropShadow(shadow) => {
                assert!(matches!(shadow.color, FilterColor::CurrentColor));
                assert!((shadow.offset_x.to_px() - 2.0).abs() < 0.01);
                assert!((shadow.offset_y.to_px() - 3.0).abs() < 0.01);
            }
            _ => panic!("expected drop-shadow"),
        }
    }

    #[test]
    fn parses_backdrop_filter_as_filter_list() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "backdrop-filter".to_string(),
            value: PropertyValue::Keyword("blur(5px)".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.backdrop_filter.len(), 1);
        match &style.backdrop_filter[0] {
            FilterFunction::Blur(len) => assert!((len.to_px() - 5.0).abs() < 0.01),
            _ => panic!("expected blur"),
        }
    }

    #[test]
    fn parses_tab_size_number_and_length() {
        let mut style = ComputedStyle::default();
        let number_decl = Declaration {
            property: "tab-size".to_string(),
            value: PropertyValue::Number(4.0),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &number_decl, 16.0, 16.0);
        assert!(matches!(style.tab_size, TabSize::Number(n) if (n - 4.0).abs() < 0.001));

        let length_decl = Declaration {
            property: "tab-size".to_string(),
            value: PropertyValue::Length(Length::px(20.0)),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &length_decl, 16.0, 16.0);
        match style.tab_size {
            TabSize::Length(len) => assert!((len.to_px() - 20.0).abs() < 0.001),
            other => panic!("expected length tab size, got {:?}", other),
        }
    }

    #[test]
    fn parses_font_shorthand_with_style_weight_size_line_height_and_family() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font".to_string(),
            value: PropertyValue::Keyword("italic 700 20px/30px \"Fira Sans\", serif".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_style, FontStyle::Italic));
        assert!(matches!(style.font_weight, FontWeight::Number(700)));
        assert!(matches!(
            style.font_stretch,
            FontStretch::Normal | FontStretch::Percentage(_)
        ));
        assert!((style.font_size - 20.0).abs() < 0.01);
        match style.line_height {
            LineHeight::Length(len) => assert!((len.to_px() - 30.0).abs() < 0.01),
            _ => panic!("expected length line-height"),
        }
        assert_eq!(style.font_family, vec!["Fira Sans".to_string(), "serif".to_string()]);
    }

    #[test]
    fn parses_font_shorthand_with_relative_size_and_percentage_line_height() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font".to_string(),
            value: PropertyValue::Keyword("bold larger/125% serif".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 20.0, 16.0);
        assert!(matches!(style.font_weight, FontWeight::Bold));
        assert!((style.font_stretch.to_percentage() - 100.0).abs() < 0.01);
        assert!((style.font_size - 24.0).abs() < 0.01);
        assert!(matches!(style.line_height, LineHeight::Number(n) if (n - 1.25).abs() < 0.001));
        assert_eq!(style.font_family, vec!["serif".to_string()]);
    }

    #[test]
    fn font_shorthand_without_family_is_ignored() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font".to_string(),
            value: PropertyValue::Keyword("italic 16px".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        // Defaults stay intact because the shorthand is invalid.
        assert!(matches!(style.font_style, FontStyle::Normal));
        assert!((style.font_size - 16.0).abs() < 0.01);
    }

    #[test]
    fn line_height_percentage_is_resolved_to_number() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "line-height".to_string(),
            value: PropertyValue::Percentage(150.0),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.line_height, LineHeight::Number(n) if (n - 1.5).abs() < 0.001));
    }

    #[test]
    fn font_size_keywords_and_percentages_resolve_against_parent() {
        let mut style = ComputedStyle::default();

        let decl = Declaration {
            property: "font-size".to_string(),
            value: PropertyValue::Keyword("large".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.font_size - 19.2).abs() < 0.01);

        let decl = Declaration {
            property: "font-size".to_string(),
            value: PropertyValue::Percentage(150.0),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 20.0, 16.0);
        assert!((style.font_size - 30.0).abs() < 0.01);

        let decl = Declaration {
            property: "font-size".to_string(),
            value: PropertyValue::Length(Length::em(2.0)),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 10.0, 16.0);
        assert!((style.font_size - 20.0).abs() < 0.01);
    }

    #[test]
    fn parses_font_stretch_longhand_keywords_and_percentages() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-stretch".to_string(),
            value: PropertyValue::Keyword("expanded".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_stretch, FontStretch::Expanded));

        let decl = Declaration {
            property: "font-stretch".to_string(),
            value: PropertyValue::Percentage(125.0),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.font_stretch.to_percentage() - 125.0).abs() < 0.01);
    }

    #[test]
    fn font_shorthand_accepts_font_stretch_keyword() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font".to_string(),
            value: PropertyValue::Keyword("italic bold condensed 16px/20px serif".to_string()),
            raw_value: String::new(),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.font_stretch.to_percentage() - FontStretch::Condensed.to_percentage()).abs() < 0.01);
        assert!(matches!(style.font_weight, FontWeight::Bold));
        assert!(matches!(style.font_style, FontStyle::Italic));
        assert!((style.font_size - 16.0).abs() < 0.01);
        assert!(matches!(style.line_height, LineHeight::Length(_)));
    }

    #[test]
    fn parses_font_variant_longhand_and_shorthand() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-variant".to_string(),
            value: PropertyValue::Keyword("small-caps".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant, FontVariant::SmallCaps));

        let decl = Declaration {
            property: "font".to_string(),
            value: PropertyValue::Keyword("small-caps 16px serif".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant, FontVariant::SmallCaps));
    }

    #[test]
    fn parses_font_variant_caps_longhand() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-variant-caps".to_string(),
            value: PropertyValue::Keyword("all-small-caps".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps));

        let decl = Declaration {
            property: "font-variant".to_string(),
            value: PropertyValue::Keyword("all-petite-caps titling-caps".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_caps, FontVariantCaps::TitlingCaps));
    }

    #[test]
    fn parses_font_variant_position() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-variant-position".to_string(),
            value: PropertyValue::Keyword("super".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_position, FontVariantPosition::Super));

        let decl = Declaration {
            property: "font-variant-position".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_position, FontVariantPosition::Normal));
    }

    #[test]
    fn parses_font_size_adjust() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-size-adjust".to_string(),
            value: PropertyValue::Number(0.7),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_size_adjust, FontSizeAdjust::Number(v) if (v - 0.7).abs() < 1e-6));

        let decl = Declaration {
            property: "font-size-adjust".to_string(),
            value: PropertyValue::Keyword("from-font".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_size_adjust, FontSizeAdjust::FromFont));

        let decl = Declaration {
            property: "font-size-adjust".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_size_adjust, FontSizeAdjust::None));
    }

    #[test]
    fn parses_font_synthesis() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-synthesis".to_string(),
            value: PropertyValue::Keyword("weight style".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.font_synthesis.weight);
        assert!(style.font_synthesis.style);
        assert!(!style.font_synthesis.small_caps);

        let decl = Declaration {
            property: "font-synthesis".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(!style.font_synthesis.weight);
        assert!(!style.font_synthesis.style);
        assert!(!style.font_synthesis.small_caps);
    }

    #[test]
    fn parses_font_variant_east_asian() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-variant-east-asian".to_string(),
            value: PropertyValue::Keyword("jis90 proportional-width ruby".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(
            style.font_variant_east_asian.variant,
            Some(EastAsianVariant::Jis90)
        ));
        assert!(matches!(
            style.font_variant_east_asian.width,
            Some(EastAsianWidth::ProportionalWidth)
        ));
        assert!(style.font_variant_east_asian.ruby);

        let decl = Declaration {
            property: "font-variant-east-asian".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.font_variant_east_asian.variant.is_none());
        assert!(style.font_variant_east_asian.width.is_none());
        assert!(!style.font_variant_east_asian.ruby);
    }

    #[test]
    fn parses_font_variant_numeric_longhand() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-variant-numeric".to_string(),
            value: PropertyValue::Keyword(
                "oldstyle-nums tabular-nums stacked-fractions ordinal slashed-zero".to_string(),
            ),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_numeric.figure, NumericFigure::Oldstyle));
        assert!(matches!(style.font_variant_numeric.spacing, NumericSpacing::Tabular));
        assert!(matches!(style.font_variant_numeric.fraction, NumericFraction::Stacked));
        assert!(style.font_variant_numeric.ordinal);
        assert!(style.font_variant_numeric.slashed_zero);

        let decl = Declaration {
            property: "font-variant-numeric".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_numeric.figure, NumericFigure::Normal));
        assert!(matches!(style.font_variant_numeric.spacing, NumericSpacing::Normal));
        assert!(matches!(style.font_variant_numeric.fraction, NumericFraction::Normal));
        assert!(!style.font_variant_numeric.ordinal);
        assert!(!style.font_variant_numeric.slashed_zero);
    }

    #[test]
    fn parses_font_kerning() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "font-kerning".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_kerning, FontKerning::None));

        let decl = Declaration {
            property: "font-kerning".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_kerning, FontKerning::Normal));
    }

    #[test]
    fn parses_float_and_clear() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "float".to_string(),
            value: PropertyValue::Keyword("left".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.float, crate::style::float::Float::Left));

        let decl = Declaration {
            property: "clear".to_string(),
            value: PropertyValue::Keyword("both".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.clear, crate::style::float::Clear::Both));

        let decl = Declaration {
            property: "float".to_string(),
            value: PropertyValue::Keyword("invalid".to_string()),
            raw_value: String::new(),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        // Invalid value leaves previous value unchanged
        assert!(matches!(style.float, crate::style::float::Float::Left));
    }
}
#[derive(Default)]
struct BackgroundShorthand {
    color: Option<Rgba>,
    image: Option<BackgroundImage>,
    repeat: Option<BackgroundRepeat>,
    position: Option<BackgroundPosition>,
    size: Option<BackgroundSize>,
    attachment: Option<BackgroundAttachment>,
    origin: Option<BackgroundBox>,
    clip: Option<BackgroundBox>,
}

fn parse_background_shorthand(tokens: &[PropertyValue], current_color: Rgba) -> Option<BackgroundShorthand> {
    if tokens.is_empty() {
        return None;
    }

    let mut shorthand = BackgroundShorthand::default();

    // Split position/size by `/` if present
    let mut slash_idx = None;
    let mut size_end = tokens.len();
    if let Some(idx) = tokens
        .iter()
        .position(|t| matches!(t, PropertyValue::Keyword(k) if k == "/"))
    {
        slash_idx = Some(idx);
        let mut size_tokens: Vec<PropertyValue> = Vec::new();
        let mut cursor = idx + 1;
        while cursor < tokens.len() {
            let t = &tokens[cursor];
            let is_size_token = match t {
                PropertyValue::Length(_) | PropertyValue::Percentage(_) => true,
                PropertyValue::Number(n) if *n == 0.0 => true,
                PropertyValue::Keyword(k) if k == "auto" || k == "cover" || k == "contain" => true,
                _ => false,
            };
            if is_size_token {
                size_tokens.push(t.clone());
                cursor += 1;
            } else {
                break;
            }
        }
        size_end = cursor;
        let pos_tokens = &tokens[..idx];
        if !pos_tokens.is_empty() {
            if let Some(pos) = parse_background_position(&PropertyValue::Multiple(pos_tokens.to_vec())) {
                shorthand.position = Some(pos);
            }
        }
        if !size_tokens.is_empty() {
            if let Some(size) = parse_background_size(&PropertyValue::Multiple(size_tokens.clone())) {
                shorthand.size = Some(size);
            }
        }
    }

    let mut boxes: Vec<BackgroundBox> = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        if slash_idx == Some(idx) {
            idx = size_end;
            continue;
        }

        let token = &tokens[idx];

        // Color
        if shorthand.color.is_none() {
            if let PropertyValue::Color(c) = token {
                shorthand.color = Some(*c);
                idx += 1;
                continue;
            } else if let PropertyValue::Keyword(kw) = token {
                if kw.eq_ignore_ascii_case("currentcolor") {
                    shorthand.color = Some(current_color);
                    idx += 1;
                    continue;
                }
            }
        }

        // Image
        if shorthand.image.is_none() {
            match token {
                PropertyValue::Url(url) => {
                    shorthand.image = Some(BackgroundImage::Url(url.clone()));
                    idx += 1;
                    continue;
                }
                PropertyValue::LinearGradient { angle, stops } => {
                    shorthand.image = Some(BackgroundImage::LinearGradient {
                        angle: *angle,
                        stops: stops.clone(),
                    });
                    idx += 1;
                    continue;
                }
                PropertyValue::RadialGradient {
                    shape,
                    size,
                    position,
                    stops,
                } => {
                    shorthand.image = Some(BackgroundImage::RadialGradient {
                        shape: *shape,
                        size: size.clone(),
                        position: BackgroundPosition::Position {
                            x: BackgroundPositionComponent {
                                alignment: position.x.alignment,
                                offset: position.x.offset,
                            },
                            y: BackgroundPositionComponent {
                                alignment: position.y.alignment,
                                offset: position.y.offset,
                            },
                        },
                        stops: stops.clone(),
                    });
                    idx += 1;
                    continue;
                }
                PropertyValue::RepeatingLinearGradient { angle, stops } => {
                    shorthand.image = Some(BackgroundImage::RepeatingLinearGradient {
                        angle: *angle,
                        stops: stops.clone(),
                    });
                    idx += 1;
                    continue;
                }
                PropertyValue::RepeatingRadialGradient {
                    shape,
                    size,
                    position,
                    stops,
                } => {
                    shorthand.image = Some(BackgroundImage::RepeatingRadialGradient {
                        shape: *shape,
                        size: size.clone(),
                        position: BackgroundPosition::Position {
                            x: BackgroundPositionComponent {
                                alignment: position.x.alignment,
                                offset: position.x.offset,
                            },
                            y: BackgroundPositionComponent {
                                alignment: position.y.alignment,
                                offset: position.y.offset,
                            },
                        },
                        stops: stops.clone(),
                    });
                    idx += 1;
                    continue;
                }
                PropertyValue::ConicGradient {
                    from_angle,
                    position,
                    stops,
                } => {
                    shorthand.image = Some(BackgroundImage::ConicGradient {
                        from_angle: *from_angle,
                        position: BackgroundPosition::Position {
                            x: BackgroundPositionComponent {
                                alignment: position.x.alignment,
                                offset: position.x.offset,
                            },
                            y: BackgroundPositionComponent {
                                alignment: position.y.alignment,
                                offset: position.y.offset,
                            },
                        },
                        stops: stops.clone(),
                    });
                    idx += 1;
                    continue;
                }
                PropertyValue::RepeatingConicGradient {
                    from_angle,
                    position,
                    stops,
                } => {
                    shorthand.image = Some(BackgroundImage::RepeatingConicGradient {
                        from_angle: *from_angle,
                        position: BackgroundPosition::Position {
                            x: BackgroundPositionComponent {
                                alignment: position.x.alignment,
                                offset: position.x.offset,
                            },
                            y: BackgroundPositionComponent {
                                alignment: position.y.alignment,
                                offset: position.y.offset,
                            },
                        },
                        stops: stops.clone(),
                    });
                    idx += 1;
                    continue;
                }
                PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => {
                    if let Some(img) = parse_image_set(kw) {
                        shorthand.image = Some(img);
                        idx += 1;
                        continue;
                    }
                }
                PropertyValue::Keyword(kw) if kw == "none" => {
                    shorthand.image = None;
                    idx += 1;
                    continue;
                }
                _ => {}
            }
        }

        // Repeat
        if shorthand.repeat.is_none() {
            if let Some(rep) = parse_background_repeat(token) {
                shorthand.repeat = Some(rep);
                idx += 1;
                continue;
            }
            if idx + 1 < tokens.len() {
                if let PropertyValue::Keyword(_) = token {
                    if let PropertyValue::Keyword(_) = tokens[idx + 1] {
                        let pair = PropertyValue::Multiple(vec![token.clone(), tokens[idx + 1].clone()]);
                        if let Some(rep) = parse_background_repeat(&pair) {
                            shorthand.repeat = Some(rep);
                            idx += 2;
                            continue;
                        }
                    }
                }
            }
        }

        // Attachment
        if shorthand.attachment.is_none() {
            if let PropertyValue::Keyword(kw) = token {
                shorthand.attachment = match kw.as_str() {
                    "scroll" => Some(BackgroundAttachment::Scroll),
                    "fixed" => Some(BackgroundAttachment::Fixed),
                    "local" => Some(BackgroundAttachment::Local),
                    _ => None,
                };
                if shorthand.attachment.is_some() {
                    idx += 1;
                    continue;
                }
            }
        }

        // Background boxes
        if let Some(b) = parse_background_box(token) {
            boxes.push(b);
            idx += 1;
            continue;
        }

        // Position (if not already parsed via slash)
        if shorthand.position.is_none() {
            if let Some(pos) = parse_background_position(token) {
                shorthand.position = Some(pos);
                idx += 1;
                continue;
            }
        }

        idx += 1;
    }

    if boxes.len() == 1 {
        shorthand.origin = Some(boxes[0]);
        shorthand.clip = Some(boxes[0]);
    } else if boxes.len() >= 2 {
        shorthand.origin = Some(boxes[0]);
        shorthand.clip = Some(boxes[1]);
    }

    Some(shorthand)
}
fn parse_feature_setting<'i, 't>(
    parser: &mut Parser<'i, 't>,
) -> Result<FontFeatureSetting, cssparser::ParseError<'i, ()>> {
    parser.skip_whitespace();
    let location = parser.current_source_location();

    let tag_bytes: [u8; 4] = match parser.next()? {
        Token::QuotedString(s) | Token::Ident(s) if s.len() == 4 && s.as_bytes().iter().all(|b| b.is_ascii()) => {
            s.as_bytes().try_into().map_err(|_| location.new_custom_error(()))?
        }
        _ => return Err(location.new_custom_error(())),
    };

    parser.skip_whitespace();
    let value = if let Ok(num) = parser.try_parse(|p| p.expect_number()) {
        num.max(0.0) as u32
    } else if let Ok(ident) = parser.try_parse(|p| p.expect_ident().map(|ident| ident.as_ref().to_ascii_lowercase())) {
        match ident.as_str() {
            "on" => 1,
            "off" => 0,
            _ => return Err(location.new_custom_error(())),
        }
    } else {
        1
    };

    Ok(FontFeatureSetting { tag: tag_bytes, value })
}

fn inline_axis_is_horizontal(wm: WritingMode) -> bool {
    matches!(
        wm,
        WritingMode::HorizontalTb | WritingMode::SidewaysLr | WritingMode::SidewaysRl
    )
}

fn inline_axis_positive(wm: WritingMode, dir: Direction) -> bool {
    match wm {
        WritingMode::HorizontalTb => dir != Direction::Rtl,
        WritingMode::SidewaysRl => false,
        WritingMode::SidewaysLr => true,
        WritingMode::VerticalRl | WritingMode::VerticalLr => true,
    }
}

fn block_axis_is_horizontal(wm: WritingMode) -> bool {
    matches!(wm, WritingMode::VerticalRl | WritingMode::VerticalLr)
}

fn block_axis_positive(wm: WritingMode) -> bool {
    match wm {
        WritingMode::VerticalRl => false,
        _ => true,
    }
}

fn inline_physical_sides(styles: &ComputedStyle) -> (crate::style::PhysicalSide, crate::style::PhysicalSide) {
    let horizontal = inline_axis_is_horizontal(styles.writing_mode);
    let positive = inline_axis_positive(styles.writing_mode, styles.direction);
    if horizontal {
        if positive {
            (crate::style::PhysicalSide::Left, crate::style::PhysicalSide::Right)
        } else {
            (crate::style::PhysicalSide::Right, crate::style::PhysicalSide::Left)
        }
    } else if positive {
        (crate::style::PhysicalSide::Top, crate::style::PhysicalSide::Bottom)
    } else {
        (crate::style::PhysicalSide::Bottom, crate::style::PhysicalSide::Top)
    }
}

fn block_physical_sides(styles: &ComputedStyle) -> (crate::style::PhysicalSide, crate::style::PhysicalSide) {
    let horizontal = block_axis_is_horizontal(styles.writing_mode);
    let positive = block_axis_positive(styles.writing_mode);
    if horizontal {
        if positive {
            (crate::style::PhysicalSide::Left, crate::style::PhysicalSide::Right)
        } else {
            (crate::style::PhysicalSide::Right, crate::style::PhysicalSide::Left)
        }
    } else if positive {
        (crate::style::PhysicalSide::Top, crate::style::PhysicalSide::Bottom)
    } else {
        (crate::style::PhysicalSide::Bottom, crate::style::PhysicalSide::Top)
    }
}

fn sides_for_axis(
    axis: crate::style::LogicalAxis,
    inline_sides: (crate::style::PhysicalSide, crate::style::PhysicalSide),
    block_sides: (crate::style::PhysicalSide, crate::style::PhysicalSide),
) -> (crate::style::PhysicalSide, crate::style::PhysicalSide) {
    match axis {
        crate::style::LogicalAxis::Inline => inline_sides,
        crate::style::LogicalAxis::Block => block_sides,
    }
}

fn corner_from_logical_sides(
    block_side: crate::style::PhysicalSide,
    inline_side: crate::style::PhysicalSide,
) -> Option<PhysicalCorner> {
    let vertical = match (block_side, inline_side) {
        (crate::style::PhysicalSide::Top, _) | (crate::style::PhysicalSide::Bottom, _) => Some(block_side),
        (_, crate::style::PhysicalSide::Top) | (_, crate::style::PhysicalSide::Bottom) => Some(inline_side),
        _ => None,
    }?;
    let horizontal = match (block_side, inline_side) {
        (crate::style::PhysicalSide::Left, _) | (crate::style::PhysicalSide::Right, _) => Some(block_side),
        (_, crate::style::PhysicalSide::Left) | (_, crate::style::PhysicalSide::Right) => Some(inline_side),
        _ => None,
    }?;

    match (vertical, horizontal) {
        (crate::style::PhysicalSide::Top, crate::style::PhysicalSide::Left) => Some(PhysicalCorner::TopLeft),
        (crate::style::PhysicalSide::Top, crate::style::PhysicalSide::Right) => Some(PhysicalCorner::TopRight),
        (crate::style::PhysicalSide::Bottom, crate::style::PhysicalSide::Left) => Some(PhysicalCorner::BottomLeft),
        (crate::style::PhysicalSide::Bottom, crate::style::PhysicalSide::Right) => Some(PhysicalCorner::BottomRight),
        _ => None,
    }
}

pub fn resolve_pending_logical_properties(styles: &mut ComputedStyle) {
    if styles.logical.pending.is_empty() {
        return;
    }

    let inline_sides = inline_physical_sides(styles);
    let block_sides = block_physical_sides(styles);

    let pending = std::mem::take(&mut styles.logical.pending);
    for pending_prop in pending {
        match pending_prop.property {
            crate::style::LogicalProperty::Margin { axis, start, end } => {
                let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
                if let Some(v) = start {
                    set_margin_side(styles, start_side, v, pending_prop.order);
                }
                if let Some(v) = end {
                    set_margin_side(styles, end_side, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::Padding { axis, start, end } => {
                let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
                if let Some(v) = start {
                    set_padding_side(styles, start_side, v, pending_prop.order);
                }
                if let Some(v) = end {
                    set_padding_side(styles, end_side, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::BorderWidth { axis, start, end } => {
                let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
                if let Some(v) = start {
                    set_border_width_side(styles, start_side, v, pending_prop.order);
                }
                if let Some(v) = end {
                    set_border_width_side(styles, end_side, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::BorderStyle { axis, start, end } => {
                let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
                if let Some(v) = start {
                    set_border_style_side(styles, start_side, v, pending_prop.order);
                }
                if let Some(v) = end {
                    set_border_style_side(styles, end_side, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::BorderColor { axis, start, end } => {
                let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
                if let Some(v) = start {
                    set_border_color_side(styles, start_side, v, pending_prop.order);
                }
                if let Some(v) = end {
                    set_border_color_side(styles, end_side, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::InlineSize { value } => {
                if let Some(v) = value {
                    set_axis_dimension(styles, crate::style::LogicalAxis::Inline, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::BlockSize { value } => {
                if let Some(v) = value {
                    set_axis_dimension(styles, crate::style::LogicalAxis::Block, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::MinInlineSize { value } => {
                if let Some(v) = value {
                    set_axis_min_dimension(styles, crate::style::LogicalAxis::Inline, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::MinBlockSize { value } => {
                if let Some(v) = value {
                    set_axis_min_dimension(styles, crate::style::LogicalAxis::Block, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::MaxInlineSize { value } => {
                if let Some(v) = value {
                    set_axis_max_dimension(styles, crate::style::LogicalAxis::Inline, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::MaxBlockSize { value } => {
                if let Some(v) = value {
                    set_axis_max_dimension(styles, crate::style::LogicalAxis::Block, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::Inset { axis, start, end } => {
                let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
                if let Some(v) = start {
                    set_inset_side(styles, start_side, v, pending_prop.order);
                }
                if let Some(v) = end {
                    set_inset_side(styles, end_side, v, pending_prop.order);
                }
            }
            crate::style::LogicalProperty::BorderCorner {
                block_start,
                inline_start,
                value,
            } => {
                let block_side = if block_start { block_sides.0 } else { block_sides.1 };
                let inline_side = if inline_start { inline_sides.0 } else { inline_sides.1 };
                if let Some(corner) = corner_from_logical_sides(block_side, inline_side) {
                    set_corner_radius(styles, corner, value, pending_prop.order);
                }
            }
        }
    }
}
