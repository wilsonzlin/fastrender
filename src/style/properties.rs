//! CSS Property Application
//!
//! This module handles applying individual CSS property declarations
//! to a computed style struct.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::types::{Declaration, PropertyValue};
use crate::style::color::Rgba;
use crate::style::counters::CounterSet;
use crate::style::display::Display;
use crate::style::float::{Clear, Float};
use crate::style::grid::parse_grid_tracks_with_names;
use crate::style::position::Position;
use crate::style::types::*;
use crate::style::values::{Length, LengthUnit};
use crate::style::var_resolution::resolve_var;
use crate::style::ComputedStyle;
use cssparser::{Parser, ParserInput, Token};

pub fn apply_declaration(styles: &mut ComputedStyle, decl: &Declaration, parent_font_size: f32, root_font_size: f32) {
    // Handle CSS Custom Properties (--*)
    if decl.property.starts_with("--") {
        // Convert the property value to a string for storage
        let value_str = match &decl.value {
            PropertyValue::Keyword(kw) => kw.clone(),
            PropertyValue::Length(len) => {
                use crate::style::values::LengthUnit;
                format!(
                    "{}{}",
                    len.value,
                    match len.unit {
                        LengthUnit::Px => "px",
                        LengthUnit::Em => "em",
                        LengthUnit::Rem => "rem",
                        LengthUnit::Percent => "%",
                        LengthUnit::Pt => "pt",
                        LengthUnit::Vw => "vw",
                        LengthUnit::Vh => "vh",
                        LengthUnit::Cm => "cm",
                        LengthUnit::Mm => "mm",
                        LengthUnit::In => "in",
                        LengthUnit::Pc => "pc",
                        _ => "px",
                    }
                )
            }
            PropertyValue::Number(n) => n.to_string(),
            PropertyValue::Percentage(p) => format!("{}%", p),
            PropertyValue::Color(c) => format!("#{:02x}{:02x}{:02x}", c.r, c.g, c.b),
            _ => return, // Skip other types for now
        };
        styles.custom_properties.insert(decl.property.clone(), value_str);
        return;
    }

    // Resolve var() references in the value
    let resolved_value = resolve_var(&decl.value, &styles.custom_properties);

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

        "top" => styles.top = extract_length(&resolved_value),
        "right" => styles.right = extract_length(&resolved_value),
        "bottom" => styles.bottom = extract_length(&resolved_value),
        "left" => styles.left = extract_length(&resolved_value),
        "z-index" => {
            if let PropertyValue::Number(n) = resolved_value {
                styles.z_index = n as i32;
            }
        }
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
        "width" => styles.width = extract_length(&resolved_value),
        "height" => styles.height = extract_length(&resolved_value),
        "min-width" => styles.min_width = extract_length(&resolved_value),
        "min-height" => styles.min_height = extract_length(&resolved_value),
        "max-width" => styles.max_width = extract_length(&resolved_value),
        "max-height" => styles.max_height = extract_length(&resolved_value),

        // Margin
        "margin" => {
            if let Some(lengths) = extract_margin_values(&resolved_value) {
                apply_margin_values(
                    &mut styles.margin_top,
                    &mut styles.margin_right,
                    &mut styles.margin_bottom,
                    &mut styles.margin_left,
                    lengths,
                );
            }
        }
        "margin-top" => {
            styles.margin_top = extract_length(&resolved_value);
        }
        "margin-right" => {
            styles.margin_right = extract_length(&resolved_value);
        }
        "margin-bottom" => {
            styles.margin_bottom = extract_length(&resolved_value);
        }
        "margin-left" => {
            styles.margin_left = extract_length(&resolved_value);
        }

        // Padding
        "padding" => {
            if let Some(lengths) = extract_box_values(&resolved_value) {
                apply_box_values(
                    &mut styles.padding_top,
                    &mut styles.padding_right,
                    &mut styles.padding_bottom,
                    &mut styles.padding_left,
                    lengths,
                );
            }
        }
        "padding-top" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_top = len;
            }
        }
        "padding-right" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_right = len;
            }
        }
        "padding-bottom" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_bottom = len;
            }
        }
        "padding-left" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.padding_left = len;
            }
        }

        // Border width
        "border-width" => {
            if let Some(lengths) = extract_box_values(&resolved_value) {
                apply_box_values(
                    &mut styles.border_top_width,
                    &mut styles.border_right_width,
                    &mut styles.border_bottom_width,
                    &mut styles.border_left_width,
                    lengths,
                );
            }
        }
        "border-top-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_width = len;
            }
        }
        "border-right-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_right_width = len;
            }
        }
        "border-bottom-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_bottom_width = len;
            }
        }
        "border-left-width" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_left_width = len;
            }
        }

        // Border color
        "border-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_top_color = c;
                styles.border_right_color = c;
                styles.border_bottom_color = c;
                styles.border_left_color = c;
            }
        }
        "border-top-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_top_color = c;
            }
        }
        "border-right-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_right_color = c;
            }
        }
        "border-bottom-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_bottom_color = c;
            }
        }
        "border-left-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.border_left_color = c;
            }
        }

        // Border style
        "border-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let style = parse_border_style(kw);
                styles.border_top_style = style;
                styles.border_right_style = style;
                styles.border_bottom_style = style;
                styles.border_left_style = style;
            }
        }
        "border-top-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_top_style = parse_border_style(kw);
            }
        }
        "border-right-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_right_style = parse_border_style(kw);
            }
        }
        "border-bottom-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_bottom_style = parse_border_style(kw);
            }
        }
        "border-left-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.border_left_style = parse_border_style(kw);
            }
        }

        // Border (shorthand)
        "border" => {
            if let PropertyValue::Multiple(values) = &resolved_value {
                for val in values {
                    match val {
                        PropertyValue::Length(len) => {
                            styles.border_top_width = *len;
                            styles.border_right_width = *len;
                            styles.border_bottom_width = *len;
                            styles.border_left_width = *len;
                        }
                        PropertyValue::Keyword(kw) => {
                            let style = parse_border_style(kw);
                            styles.border_top_style = style;
                            styles.border_right_style = style;
                            styles.border_bottom_style = style;
                            styles.border_left_style = style;
                        }
                        PropertyValue::Color(c) => {
                            styles.border_top_color = *c;
                            styles.border_right_color = *c;
                            styles.border_bottom_color = *c;
                            styles.border_left_color = *c;
                        }
                        _ => {}
                    }
                }
            }
        }

        // Border radius
        "border-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_left_radius = len;
                styles.border_top_right_radius = len;
                styles.border_bottom_left_radius = len;
                styles.border_bottom_right_radius = len;
            }
        }
        "border-top-left-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_left_radius = len;
            }
        }
        "border-top-right-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_top_right_radius = len;
            }
        }
        "border-bottom-left-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_bottom_left_radius = len;
            }
        }
        "border-bottom-right-radius" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.border_bottom_right_radius = len;
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
                styles.align_items = match kw.as_str() {
                    "flex-start" | "start" => AlignItems::FlexStart,
                    "flex-end" | "end" => AlignItems::FlexEnd,
                    "center" => AlignItems::Center,
                    "baseline" => AlignItems::Baseline,
                    "stretch" => AlignItems::Stretch,
                    _ => styles.align_items,
                };
            }
        }
        "align-content" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.align_content = match kw.as_str() {
                    "flex-start" | "start" => AlignContent::FlexStart,
                    "flex-end" | "end" => AlignContent::FlexEnd,
                    "center" => AlignContent::Center,
                    "space-between" => AlignContent::SpaceBetween,
                    "space-around" => AlignContent::SpaceAround,
                    "stretch" => AlignContent::Stretch,
                    _ => styles.align_content,
                };
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
                let (tracks, named_lines) = parse_grid_tracks_with_names(kw);
                styles.grid_template_columns = tracks;
                styles.grid_column_names = named_lines;
            }
        }
        "grid-template-rows" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let (tracks, named_lines) = parse_grid_tracks_with_names(kw);
                styles.grid_template_rows = tracks;
                styles.grid_row_names = named_lines;
            }
        }
        "grid-gap" | "gap" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.grid_gap = len;
                styles.grid_row_gap = len;
                styles.grid_column_gap = len;
            }
        }
        "grid-row-gap" | "row-gap" => {
            if let Some(len) = extract_length(&resolved_value) {
                styles.grid_row_gap = len;
            }
        }
        "grid-column-gap" | "column-gap" => {
            if let Some(len) = extract_length(&resolved_value) {
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
                        TextAlign::MatchParent => TextAlignLast::Auto,
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

        // Color
        "color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.color = c;
            }
        }
        "background-color" => {
            if let PropertyValue::Color(c) = resolved_value {
                styles.background_color = c;
            }
        }

        // Background
        "background-image" => match &resolved_value {
            PropertyValue::Url(url) => {
                styles.background_image = Some(BackgroundImage::Url(url.clone()));
            }
            PropertyValue::LinearGradient { angle, stops } => {
                styles.background_image = Some(BackgroundImage::LinearGradient {
                    angle: *angle,
                    stops: stops.clone(),
                });
            }
            PropertyValue::RadialGradient { stops } => {
                styles.background_image = Some(BackgroundImage::RadialGradient { stops: stops.clone() });
            }
            PropertyValue::RepeatingLinearGradient { angle, stops } => {
                styles.background_image = Some(BackgroundImage::RepeatingLinearGradient {
                    angle: *angle,
                    stops: stops.clone(),
                });
            }
            PropertyValue::RepeatingRadialGradient { stops } => {
                styles.background_image = Some(BackgroundImage::RepeatingRadialGradient { stops: stops.clone() });
            }
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.background_image = None;
            }
            _ => {}
        },
        "background-size" => {
            if let Some(size) = parse_background_size(&resolved_value) {
                styles.background_size = size;
            }
        }
        "background-repeat" => {
            if let Some(rep) = parse_background_repeat(&resolved_value) {
                styles.background_repeat = rep;
            }
        }
        "background-position" => {
            if let Some(pos) = parse_background_position(&resolved_value) {
                styles.background_position = pos;
            }
        }
        "background-attachment" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.background_attachment = match kw.as_str() {
                    "scroll" => BackgroundAttachment::Scroll,
                    "fixed" => BackgroundAttachment::Fixed,
                    "local" => BackgroundAttachment::Local,
                    _ => styles.background_attachment,
                };
            }
        }
        "background-origin" => {
            if let Some(origin) = parse_background_box(&resolved_value) {
                styles.background_origin = origin;
            }
        }
        "background-clip" => {
            if let Some(clip) = parse_background_box(&resolved_value) {
                styles.background_clip = clip;
            }
        }

        // Shorthand: background (treat as background-color for now)
        "background" => {
            let mut reset_background_fields = || {
                styles.background_color = Rgba::TRANSPARENT;
                styles.background_image = None;
                styles.background_repeat = BackgroundRepeat::repeat();
                styles.background_position = BackgroundPosition::Position {
                    x: crate::style::types::BackgroundPositionComponent {
                        alignment: 0.0,
                        offset: Length::px(0.0),
                    },
                    y: crate::style::types::BackgroundPositionComponent {
                        alignment: 0.0,
                        offset: Length::px(0.0),
                    },
                };
                styles.background_size =
                    BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto);
                styles.background_attachment = BackgroundAttachment::Scroll;
                styles.background_origin = BackgroundBox::PaddingBox;
                styles.background_clip = BackgroundBox::BorderBox;
            };

            let tokens: Vec<PropertyValue> = match resolved_value {
                PropertyValue::Multiple(ref parts) => parts.clone(),
                _ => vec![resolved_value.clone()],
            };

            if !tokens.is_empty() {
                reset_background_fields();
                if let Some(parsed) = parse_background_shorthand(&tokens) {
                    if let Some(color) = parsed.color {
                        styles.background_color = color;
                    }
                    if let Some(image) = parsed.image {
                        styles.background_image = Some(image);
                    }
                    if let Some(rep) = parsed.repeat {
                        styles.background_repeat = rep;
                    }
                    if let Some(pos) = parsed.position {
                        styles.background_position = pos;
                    }
                    if let Some(size) = parsed.size {
                        styles.background_size = size;
                    }
                    if let Some(att) = parsed.attachment {
                        styles.background_attachment = att;
                    }
                    if let Some(origin) = parsed.origin {
                        styles.background_origin = origin;
                    }
                    if let Some(clip) = parsed.clip {
                        styles.background_clip = clip;
                    }
                }
            }
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
            match &resolved_value {
                PropertyValue::String(s) => {
                    styles.content = s.clone();
                }
                PropertyValue::Keyword(kw) => {
                    // "none" and "normal" mean no content
                    styles.content = kw.clone();
                }
                _ => {}
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
            let unit = unit.as_ref();
            let len = match unit {
                "px" => Length::px(*value),
                "em" => Length::em(*value),
                "rem" => Length::rem(*value),
                "pt" => Length::pt(*value),
                "pc" => Length::pc(*value),
                "in" => Length::inches(*value),
                "cm" => Length::cm(*value),
                "mm" => Length::mm(*value),
                _ => return Err(location.new_custom_error(())),
            };
            Ok(len)
        }
        Token::Percentage { unit_value, .. } => Ok(Length::percent(*unit_value)),
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

    let parsed = csscolorparser::parse(&raw).map_err(|_| location.new_custom_error(()))?;
    Ok(FilterColor::Color(Rgba::new(
        (parsed.r * 255.0).round().clamp(0.0, 255.0) as u8,
        (parsed.g * 255.0).round().clamp(0.0, 255.0) as u8,
        (parsed.b * 255.0).round().clamp(0.0, 255.0) as u8,
        parsed.a as f32,
    )))
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
    enum Axis {
        Horizontal,
        Vertical,
    }

    #[derive(Clone, Copy)]
    struct TempComponent {
        alignment: f32,
        offset: Option<Length>,
        from_keyword: bool,
    }

    let components: Vec<&PropertyValue> = match value {
        PropertyValue::Multiple(values) if !values.is_empty() => values.iter().collect(),
        _ => vec![value],
    };

    let mut x: Option<TempComponent> = None;
    let mut y: Option<TempComponent> = None;
    for comp in components.iter().take(4) {
        match comp {
            PropertyValue::Keyword(kw) => {
                let axis = match kw.as_str() {
                    "left" => Some((Axis::Horizontal, 0.0)),
                    "right" => Some((Axis::Horizontal, 1.0)),
                    "top" => Some((Axis::Vertical, 0.0)),
                    "bottom" => Some((Axis::Vertical, 1.0)),
                    "center" => {
                        if x.is_none() {
                            Some((Axis::Horizontal, 0.5))
                        } else if y.is_none() {
                            Some((Axis::Vertical, 0.5))
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                if let Some((axis, align)) = axis {
                    let target = match axis {
                        Axis::Horizontal => &mut x,
                        Axis::Vertical => &mut y,
                    };
                    if target.is_none() {
                        *target = Some(TempComponent {
                            alignment: align,
                            offset: None,
                            from_keyword: true,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    // Second pass (still sequential to respect proximity) for offsets/explicit lengths.
    let mut last_axis: Option<Axis> = None;
    for comp in components.iter().take(4) {
        match comp {
            PropertyValue::Keyword(kw) => {
                last_axis = match kw.as_str() {
                    "left" | "right" => Some(Axis::Horizontal),
                    "top" | "bottom" => Some(Axis::Vertical),
                    "center" => last_axis,
                    _ => last_axis,
                };
            }
            PropertyValue::Length(l) => {
                let len = *l;
                let handled = if let Some(axis) = last_axis {
                    let target = match axis {
                        Axis::Horizontal => &mut x,
                        Axis::Vertical => &mut y,
                    };
                    if let Some(ref mut comp) = target {
                        if comp.from_keyword && comp.offset.is_none() {
                            let mut adjusted = len;
                            if (comp.alignment - 1.0).abs() < 1e-6 {
                                adjusted.value = -adjusted.value;
                            }
                            comp.offset = Some(adjusted);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if handled {
                    continue;
                }

                if x.is_none() {
                    x = Some(TempComponent {
                        alignment: 0.0,
                        offset: Some(len),
                        from_keyword: false,
                    });
                    last_axis = Some(Axis::Horizontal);
                } else if y.is_none() {
                    y = Some(TempComponent {
                        alignment: 0.0,
                        offset: Some(len),
                        from_keyword: false,
                    });
                    last_axis = Some(Axis::Vertical);
                }
            }
            PropertyValue::Percentage(p) => {
                let len = Length::percent(*p);
                let handled = if let Some(axis) = last_axis {
                    let target = match axis {
                        Axis::Horizontal => &mut x,
                        Axis::Vertical => &mut y,
                    };
                    if let Some(ref mut comp) = target {
                        if comp.from_keyword && comp.offset.is_none() {
                            let mut adjusted = len;
                            if (comp.alignment - 1.0).abs() < 1e-6 {
                                adjusted.value = -adjusted.value;
                            }
                            comp.offset = Some(adjusted);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if handled {
                    continue;
                }

                if x.is_none() {
                    x = Some(TempComponent {
                        alignment: 0.0,
                        offset: Some(len),
                        from_keyword: false,
                    });
                    last_axis = Some(Axis::Horizontal);
                } else if y.is_none() {
                    y = Some(TempComponent {
                        alignment: 0.0,
                        offset: Some(len),
                        from_keyword: false,
                    });
                    last_axis = Some(Axis::Vertical);
                }
            }
            PropertyValue::Number(n) if *n == 0.0 => {
                let len = Length::px(0.0);
                let handled = if let Some(axis) = last_axis {
                    let target = match axis {
                        Axis::Horizontal => &mut x,
                        Axis::Vertical => &mut y,
                    };
                    if let Some(ref mut comp) = target {
                        if comp.from_keyword && comp.offset.is_none() {
                            comp.offset = Some(len);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if handled {
                    continue;
                }

                if x.is_none() {
                    x = Some(TempComponent {
                        alignment: 0.0,
                        offset: Some(len),
                        from_keyword: false,
                    });
                    last_axis = Some(Axis::Horizontal);
                } else if y.is_none() {
                    y = Some(TempComponent {
                        alignment: 0.0,
                        offset: Some(len),
                        from_keyword: false,
                    });
                    last_axis = Some(Axis::Vertical);
                }
            }
            _ => {}
        }
    }

    let x = x.unwrap_or(TempComponent {
        alignment: 0.5,
        offset: None,
        from_keyword: true,
    });
    let y = y.unwrap_or(TempComponent {
        alignment: 0.5,
        offset: None,
        from_keyword: true,
    });

    Some(BackgroundPosition::Position {
        x: BackgroundPositionComponent {
            alignment: x.alignment,
            offset: x.offset.unwrap_or_else(|| Length::px(0.0)),
        },
        y: BackgroundPositionComponent {
            alignment: y.alignment,
            offset: y.offset.unwrap_or_else(|| Length::px(0.0)),
        },
    })
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
        BackgroundRepeatKeyword, BoxSizing, FontStretch, FontVariant, ListStylePosition, ListStyleType, OutlineColor,
        OutlineStyle, PositionComponent, PositionKeyword, TextDecorationLine, TextDecorationStyle,
        TextDecorationThickness, TextEmphasisFill, TextEmphasisPosition, TextEmphasisShape, TextEmphasisStyle,
    };

    #[test]
    fn parses_object_fit_keyword() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "object-fit".to_string(),
            value: PropertyValue::Keyword("cover".to_string()),
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.object_fit, ObjectFit::Cover);
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
                important: false,
            },
            16.0,
            16.0,
        );

        assert!(matches!(style.box_sizing, BoxSizing::BorderBox));
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
            important: false,
        };

        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_position;
        assert!((x.alignment - 0.0).abs() < 0.01);
        assert!((y.alignment - 0.0).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!(y.offset.is_zero());
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_position;
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!((y.alignment - 0.0).abs() < 0.01);
        assert!((x.offset.value + 10.0).abs() < 0.01 && x.offset.unit == LengthUnit::Px);
        assert!((y.offset.value - 25.0).abs() < 0.01 && y.offset.unit == LengthUnit::Percent);

        // Default the missing axis to center when only one axis is provided.
        let decl = Declaration {
            property: "background-position".to_string(),
            value: PropertyValue::Keyword("top".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        let BackgroundPosition::Position { x, y } = style.background_position;
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.text_decoration.lines.contains(TextDecorationLine::UNDERLINE));
        assert!(style.text_decoration.lines.contains(TextDecorationLine::OVERLINE));

        let decl = Declaration {
            property: "text-decoration-style".to_string(),
            value: PropertyValue::Keyword("dashed".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.text_decoration.style, TextDecorationStyle::Dashed);

        let decl = Declaration {
            property: "text-decoration-color".to_string(),
            value: PropertyValue::Color(Rgba::BLUE),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.text_decoration.color, Some(Rgba::BLUE));

        let decl = Declaration {
            property: "text-decoration-thickness".to_string(),
            value: PropertyValue::Length(Length::px(3.0)),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::Square);

        let decl = Declaration {
            property: "list-style-position".to_string(),
            value: PropertyValue::Keyword("inside".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_position, ListStylePosition::Inside);

        let decl = Declaration {
            property: "list-style-image".to_string(),
            value: PropertyValue::Url("marker.png".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::UpperRoman);
        assert_eq!(style.list_style_position, ListStylePosition::Outside);
        assert_eq!(style.list_style_image, ListStyleImage::None);

        let decl = Declaration {
            property: "list-style".to_string(),
            value: PropertyValue::Url("img.png".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_image, ListStyleImage::Url("img.png".to_string()));
        assert_eq!(style.list_style_type, ListStyleType::Disc);
        assert_eq!(style.list_style_position, ListStylePosition::Outside);

        let decl = Declaration {
            property: "list-style".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::None);
        assert_eq!(style.list_style_image, ListStyleImage::None);

        let decl = Declaration {
            property: "list-style-type".to_string(),
            value: PropertyValue::Keyword("lower-greek".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.list_style_type, ListStyleType::LowerGreek);
    }

    #[test]
    fn parses_letter_and_word_spacing() {
        let mut style = ComputedStyle::default();
        style.font_size = 20.0;
        style.letter_spacing = 3.0;

        let decl = Declaration {
            property: "letter-spacing".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.letter_spacing, 0.0);

        let decl = Declaration {
            property: "letter-spacing".to_string(),
            value: PropertyValue::Length(Length::em(0.25)),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.letter_spacing - 5.0).abs() < 0.01);

        let decl = Declaration {
            property: "word-spacing".to_string(),
            value: PropertyValue::Percentage(50.0),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.word_spacing - 10.0).abs() < 0.01);

        let decl = Declaration {
            property: "word-spacing".to_string(),
            value: PropertyValue::Length(Length::em(-0.5)),
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
        style.background_repeat = BackgroundRepeat::repeat_x();
        style.background_attachment = BackgroundAttachment::Fixed;
        style.background_position = BackgroundPosition::Position {
            x: crate::style::types::BackgroundPositionComponent {
                alignment: 1.0,
                offset: Length::px(5.0),
            },
            y: crate::style::types::BackgroundPositionComponent {
                alignment: 1.0,
                offset: Length::px(5.0),
            },
        };
        style.background_size = BackgroundSize::Explicit(
            BackgroundSizeComponent::Length(Length::px(10.0)),
            BackgroundSizeComponent::Auto,
        );
        style.background_origin = BackgroundBox::ContentBox;
        style.background_clip = BackgroundBox::ContentBox;

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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert_eq!(style.background_color, Rgba::RED);
        assert!(matches!(style.background_image, Some(BackgroundImage::Url(ref s)) if s == "example.png"));
        assert_eq!(style.background_repeat, BackgroundRepeat::no_repeat());
        let BackgroundPosition::Position { x, y } = style.background_position;
        assert!((x.alignment - 1.0).abs() < 0.01);
        assert!(x.offset.is_zero());
        assert!((y.alignment - 0.5).abs() < 0.01);
        assert!(y.offset.is_zero());
        assert_eq!(
            style.background_size,
            BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)
        );
        assert_eq!(style.background_attachment, BackgroundAttachment::Fixed);
        assert_eq!(style.background_origin, BackgroundBox::ContentBox);
        assert_eq!(style.background_clip, BackgroundBox::PaddingBox);
    }

    #[test]
    fn background_shorthand_color_only_resets_to_initials() {
        let mut style = ComputedStyle::default();
        style.background_repeat = BackgroundRepeat::repeat_x();
        style.background_origin = BackgroundBox::ContentBox;
        style.background_clip = BackgroundBox::ContentBox;
        style.background_size = BackgroundSize::Explicit(
            BackgroundSizeComponent::Length(Length::px(12.0)),
            BackgroundSizeComponent::Auto,
        );

        let decl = Declaration {
            property: "background".to_string(),
            value: PropertyValue::Color(Rgba::RED),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert_eq!(style.background_color, Rgba::RED);
        assert!(style.background_image.is_none());
        assert_eq!(style.background_repeat, BackgroundRepeat::repeat());
        let BackgroundPosition::Position { x, y } = style.background_position;
        assert!(x.offset.is_zero() && (x.alignment - 0.0).abs() < 0.01);
        assert!(y.offset.is_zero() && (y.alignment - 0.0).abs() < 0.01);
        assert_eq!(
            style.background_size,
            BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
        );
        assert_eq!(style.background_attachment, BackgroundAttachment::Scroll);
        assert_eq!(style.background_origin, BackgroundBox::PaddingBox);
        assert_eq!(style.background_clip, BackgroundBox::BorderBox);
    }

    #[test]
    fn background_shorthand_none_resets_and_clears_image() {
        let mut style = ComputedStyle::default();
        style.background_color = Rgba::RED;
        style.background_image = Some(BackgroundImage::Url("foo.png".to_string()));
        style.background_repeat = BackgroundRepeat::repeat_x();
        style.background_origin = BackgroundBox::ContentBox;

        let decl = Declaration {
            property: "background".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);

        assert_eq!(style.background_color, Rgba::TRANSPARENT);
        assert!(style.background_image.is_none());
        assert_eq!(style.background_repeat, BackgroundRepeat::repeat());
        assert_eq!(style.background_origin, BackgroundBox::PaddingBox);
        assert_eq!(style.background_clip, BackgroundBox::BorderBox);
    }

    #[test]
    fn parses_background_repeat_keywords_and_pairs() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-repeat".to_string(),
            value: PropertyValue::Keyword("space".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.background_repeat.x, BackgroundRepeatKeyword::Space);
        assert_eq!(style.background_repeat.y, BackgroundRepeatKeyword::Space);

        let decl = Declaration {
            property: "background-repeat".to_string(),
            value: PropertyValue::Keyword("repeat-x".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.background_repeat.x, BackgroundRepeatKeyword::Repeat);
        assert_eq!(style.background_repeat.y, BackgroundRepeatKeyword::NoRepeat);

        let decl = Declaration {
            property: "background-repeat".to_string(),
            value: PropertyValue::Multiple(vec![
                PropertyValue::Keyword("space".to_string()),
                PropertyValue::Keyword("round".to_string()),
            ]),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(style.background_repeat.x, BackgroundRepeatKeyword::Space);
        assert_eq!(style.background_repeat.y, BackgroundRepeatKeyword::Round);
    }

    #[test]
    fn parses_background_size_components_and_defaults() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "background-size".to_string(),
            value: PropertyValue::Length(Length::px(25.0)),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.background_size,
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.background_size,
            BackgroundSize::Explicit(
                BackgroundSizeComponent::Auto,
                BackgroundSizeComponent::Length(Length::percent(50.0))
            )
        );

        let decl = Declaration {
            property: "background-size".to_string(),
            value: PropertyValue::Keyword("contain".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert_eq!(
            style.background_size,
            BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)
        );
    }

    #[test]
    fn parses_white_space_break_spaces() {
        let mut style = ComputedStyle::default();
        let decl = Declaration {
            property: "white-space".to_string(),
            value: PropertyValue::Keyword("break-spaces".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &origin_decl, 16.0, 16.0);
        assert_eq!(style.background_origin, BackgroundBox::ContentBox);

        let clip_decl = Declaration {
            property: "background-clip".to_string(),
            value: PropertyValue::Keyword("padding-box".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &clip_decl, 16.0, 16.0);
        assert_eq!(style.background_clip, BackgroundBox::PaddingBox);
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
            important: false,
        };
        apply_declaration(&mut style, &number_decl, 16.0, 16.0);
        assert!(matches!(style.tab_size, TabSize::Number(n) if (n - 4.0).abs() < 0.001));

        let length_decl = Declaration {
            property: "tab-size".to_string(),
            value: PropertyValue::Length(Length::px(20.0)),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!((style.font_size - 19.2).abs() < 0.01);

        let decl = Declaration {
            property: "font-size".to_string(),
            value: PropertyValue::Percentage(150.0),
            important: false,
        };
        apply_declaration(&mut style, &decl, 20.0, 16.0);
        assert!((style.font_size - 30.0).abs() < 0.01);

        let decl = Declaration {
            property: "font-size".to_string(),
            value: PropertyValue::Length(Length::em(2.0)),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_stretch, FontStretch::Expanded));

        let decl = Declaration {
            property: "font-stretch".to_string(),
            value: PropertyValue::Percentage(125.0),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant, FontVariant::SmallCaps));

        let decl = Declaration {
            property: "font".to_string(),
            value: PropertyValue::Keyword("small-caps 16px serif".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps));

        let decl = Declaration {
            property: "font-variant".to_string(),
            value: PropertyValue::Keyword("all-petite-caps titling-caps".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_variant_position, FontVariantPosition::Super));

        let decl = Declaration {
            property: "font-variant-position".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_size_adjust, FontSizeAdjust::Number(v) if (v - 0.7).abs() < 1e-6));

        let decl = Declaration {
            property: "font-size-adjust".to_string(),
            value: PropertyValue::Keyword("from-font".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_size_adjust, FontSizeAdjust::FromFont));

        let decl = Declaration {
            property: "font-size-adjust".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(style.font_synthesis.weight);
        assert!(style.font_synthesis.style);
        assert!(!style.font_synthesis.small_caps);

        let decl = Declaration {
            property: "font-synthesis".to_string(),
            value: PropertyValue::Keyword("none".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.font_kerning, FontKerning::None));

        let decl = Declaration {
            property: "font-kerning".to_string(),
            value: PropertyValue::Keyword("normal".to_string()),
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
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.float, crate::style::float::Float::Left));

        let decl = Declaration {
            property: "clear".to_string(),
            value: PropertyValue::Keyword("both".to_string()),
            important: false,
        };
        apply_declaration(&mut style, &decl, 16.0, 16.0);
        assert!(matches!(style.clear, crate::style::float::Clear::Both));

        let decl = Declaration {
            property: "float".to_string(),
            value: PropertyValue::Keyword("invalid".to_string()),
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

fn parse_background_shorthand(tokens: &[PropertyValue]) -> Option<BackgroundShorthand> {
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
                PropertyValue::RadialGradient { stops } => {
                    shorthand.image = Some(BackgroundImage::RadialGradient { stops: stops.clone() });
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
                PropertyValue::RepeatingRadialGradient { stops } => {
                    shorthand.image = Some(BackgroundImage::RepeatingRadialGradient { stops: stops.clone() });
                    idx += 1;
                    continue;
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
