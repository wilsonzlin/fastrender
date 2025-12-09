//! CSS Property Application
//!
//! This module handles applying individual CSS property declarations
//! to a computed style struct.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::types::{Declaration, PropertyValue};
use crate::style::color::Rgba;
use crate::style::display::Display;
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

        // Position
        "position" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                if let Ok(position) = Position::parse(kw) {
                    styles.position = position;
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
        "font-family" => {
            if let PropertyValue::FontFamily(families) = &resolved_value {
                styles.font_family = families.clone();
            }
        }
        "font-size" => {
            if let Some(len) = extract_length(&resolved_value) {
                // Resolve font-size against parent font size
                if len.unit.is_absolute() {
                    styles.font_size = len.to_px();
                } else if len.unit == LengthUnit::Em || len.unit == LengthUnit::Percent {
                    // Em/percent are relative to parent font size
                    styles.font_size =
                        len.value / (if len.unit == LengthUnit::Percent { 100.0 } else { 1.0 }) * parent_font_size;
                } else if len.unit == LengthUnit::Rem {
                    // Rem is relative to root font size
                    styles.font_size = len.value * root_font_size;
                }
            }
        }
        "font-weight" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                styles.font_weight = match kw.as_str() {
                    "normal" => FontWeight::Normal,
                    "bold" => FontWeight::Bold,
                    "lighter" => FontWeight::Number(300),
                    "bolder" => FontWeight::Number(700),
                    _ => styles.font_weight,
                };
            }
            PropertyValue::Number(n) => {
                styles.font_weight = FontWeight::Number(*n as u16);
            }
            _ => {}
        },
        "font-style" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.font_style = match kw.as_str() {
                    "normal" => FontStyle::Normal,
                    "italic" => FontStyle::Italic,
                    "oblique" => FontStyle::Oblique,
                    _ => styles.font_style,
                };
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
                styles.text_align = match kw.as_str() {
                    "start" => TextAlign::Start,
                    "end" => TextAlign::End,
                    "left" => TextAlign::Left,
                    "right" => TextAlign::Right,
                    "center" => TextAlign::Center,
                    "justify" => TextAlign::Justify,
                    "justify-all" => {
                        styles.text_align_last = TextAlignLast::Justify;
                        TextAlign::Justify
                    }
                    "match-parent" => TextAlign::MatchParent,
                    _ => styles.text_align,
                };
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

            let mut apply_component = |value: &PropertyValue| {
                match value {
                    PropertyValue::Length(len) => length = *len,
                    PropertyValue::Percentage(pct) => length = Length::percent(*pct),
                    PropertyValue::Keyword(kw) => match kw.as_str() {
                        "hanging" => hanging = true,
                        "each-line" => each_line = true,
                        _ => {}
                    },
                    _ => {}
                }
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
        "text-decoration" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_decoration = match kw.as_str() {
                    "none" => TextDecoration::None,
                    "underline" => TextDecoration::Underline,
                    "overline" => TextDecoration::Overline,
                    "line-through" => TextDecoration::LineThrough,
                    _ => styles.text_decoration,
                };
            }
        }
        "text-transform" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.text_transform = match kw.as_str() {
                    "none" => TextTransform::None,
                    "uppercase" => TextTransform::Uppercase,
                    "lowercase" => TextTransform::Lowercase,
                    "capitalize" => TextTransform::Capitalize,
                    _ => styles.text_transform,
                };
            }
        }
        "letter-spacing" => {
            if let Some(len) = extract_length(&resolved_value) {
                if len.unit.is_absolute() {
                    styles.letter_spacing = len.to_px();
                } else {
                    // Fallback for relative units in letter-spacing (usually small)
                    // Em units should be relative to current font size, but we don't have it finalized here easily
                    // Just use value as if px for now if not absolute, or 0
                    styles.letter_spacing = len.value;
                }
            }
        }
        "word-spacing" => {
            if let Some(len) = extract_length(&resolved_value) {
                if len.unit.is_absolute() {
                    styles.word_spacing = len.to_px();
                } else {
                    styles.word_spacing = len.value;
                }
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
                    _ => styles.white_space,
                };
            }
        }
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
            PropertyValue::Keyword(kw) if kw == "none" => {
                styles.background_image = None;
            }
            _ => {}
        },
        "background-size" => match &resolved_value {
            PropertyValue::Keyword(kw) => {
                styles.background_size = match kw.as_str() {
                    "auto" => BackgroundSize::Auto,
                    "cover" => BackgroundSize::Cover,
                    "contain" => BackgroundSize::Contain,
                    _ => styles.background_size,
                };
            }
            PropertyValue::Multiple(values) if values.len() == 2 => {
                if let (Some(w), Some(h)) = (extract_length(&values[0]), extract_length(&values[1])) {
                    styles.background_size = BackgroundSize::Length(w, h);
                }
            }
            _ => {}
        },
        "background-repeat" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                styles.background_repeat = match kw.as_str() {
                    "repeat" => BackgroundRepeat::Repeat,
                    "repeat-x" => BackgroundRepeat::RepeatX,
                    "repeat-y" => BackgroundRepeat::RepeatY,
                    "no-repeat" => BackgroundRepeat::NoRepeat,
                    _ => styles.background_repeat,
                };
            }
        }
        "background-position" => {
            if let Some(pos) = parse_background_position(&resolved_value) {
                styles.background_position = pos;
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
            if let PropertyValue::Color(c) = resolved_value {
                styles.background_color = c;
            } else if let PropertyValue::LinearGradient { angle, stops } = &resolved_value {
                styles.background_image = Some(BackgroundImage::LinearGradient {
                    angle: *angle,
                    stops: stops.clone(),
                });
            } else if let PropertyValue::RadialGradient { stops } = &resolved_value {
                styles.background_image = Some(BackgroundImage::RadialGradient { stops: stops.clone() });
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

        // Overflow
        "overflow" => {
            if let PropertyValue::Keyword(kw) = &resolved_value {
                let overflow = match kw.as_str() {
                    "visible" => Overflow::Visible,
                    "hidden" => Overflow::Hidden,
                    "scroll" => Overflow::Scroll,
                    "auto" => Overflow::Auto,
                    _ => Overflow::Visible,
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
                    _ => styles.overflow_y,
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

fn parse_background_position(value: &PropertyValue) -> Option<BackgroundPosition> {
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

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum AxisHint {
        Horizontal,
        Vertical,
        Either,
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

    for comp in components.into_iter().take(2) {
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
    Some(BackgroundPosition::Position(x, y))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::style::types::{PositionComponent, PositionKeyword};

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
        if let BackgroundPosition::Position(x, y) = style.background_position {
            assert!(x.unit.is_percentage());
            assert!((x.value - 0.0).abs() < 0.01);
            assert!(y.unit.is_percentage());
            assert!((y.value - 0.0).abs() < 0.01);
        } else {
            panic!("expected position variant");
        }
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
}
