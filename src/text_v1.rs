#![allow(unused_variables)]
#![allow(unused_mut)]
#![allow(clippy::needless_pass_by_value)]

use crate::error::{Error, Result};
use crate::style::{ComputedStyles, FontStyle, FontWeight, LineHeight, TextTransform, WhiteSpace};
use fontdb::{Database, Query, Source};
use rustybuzz::{Direction, Face, UnicodeBuffer};
use std::sync::Arc;
use ttf_parser::GlyphId;
use unicode_linebreak::{linebreaks, BreakOpportunity};

#[derive(Debug, Clone)]
pub struct TextLayout {
    pub lines: Vec<TextLine>,
    pub total_width: f32,
    pub total_height: f32,
}

#[derive(Debug, Clone)]
pub struct TextLine {
    pub glyphs: Vec<ShapedGlyph>,
    pub width: f32,
    pub height: f32,
    pub baseline: f32,
    pub x_offset: f32,
}

#[derive(Debug, Clone)]
pub struct ShapedGlyph {
    pub glyph_id: u16,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub font_face: Arc<FontFace>,
}

#[derive(Debug)]
pub struct FontFace {
    pub data: Vec<u8>,
    pub index: u32,
}

pub struct FontCache {
    db: Database,
}

impl FontCache {
    pub fn new() -> Self {
        let mut db = Database::new();
        db.load_system_fonts();
        FontCache { db }
    }

    pub fn get_font(&self, family: &[String], weight: FontWeight, style: FontStyle) -> Option<Arc<FontFace>> {
        // Map font weight to numeric value
        let weight_value = match weight {
            FontWeight::Normal => 400,
            FontWeight::Bold => 700,
            FontWeight::Number(w) => w,
            _ => 400, // Fallback
        };

        // Map font style
        let style_value = match style {
            FontStyle::Normal => fontdb::Style::Normal,
            FontStyle::Italic => fontdb::Style::Italic,
            FontStyle::Oblique => fontdb::Style::Oblique,
        };

        // Try each family in order
        for family_name in family {
            let query = Query {
                families: &[fontdb::Family::Name(family_name)],
                weight: fontdb::Weight(weight_value),
                style: style_value,
                ..Query::default()
            };

            if let Some(id) = self.db.query(&query) {
                if let Some(face_info) = self.db.face(id) {
                    // Load font data
                    let data = match &face_info.source {
                        Source::Binary(arc) => arc.as_ref().as_ref().to_vec(),
                        Source::File(path) => {
                            if let Ok(bytes) = std::fs::read(path) {
                                bytes
                            } else {
                                continue;
                            }
                        }
                        Source::SharedFile(path, _) => {
                            if let Ok(bytes) = std::fs::read(path) {
                                bytes
                            } else {
                                continue;
                            }
                        }
                    };

                    return Some(Arc::new(FontFace {
                        data,
                        index: face_info.index,
                    }));
                }
            }
        }

        // Fallback to default serif font
        let query = Query {
            families: &[fontdb::Family::Serif],
            weight: fontdb::Weight(weight_value),
            style: style_value,
            ..Query::default()
        };

        if let Some(id) = self.db.query(&query) {
            if let Some(face_info) = self.db.face(id) {
                let data = match &face_info.source {
                    Source::Binary(arc) => arc.as_ref().as_ref().to_vec(),
                    Source::File(path) => std::fs::read(path).ok()?,
                    Source::SharedFile(path, _) => std::fs::read(path).ok()?,
                };

                return Some(Arc::new(FontFace {
                    data,
                    index: face_info.index,
                }));
            }
        }

        None
    }
}

pub fn shape_text(
    text: &str,
    styles: &ComputedStyles,
    font_cache: &FontCache,
    max_width: Option<f32>,
) -> Result<TextLayout> {
    // Get font
    let font_face = font_cache
        .get_font(&styles.font_family, styles.font_weight, styles.font_style)
        .ok_or_else(|| {
            Error::Font(crate::error::FontError::LoadFailed {
                family: styles.font_family.join(", "),
                reason: "Font not found in cache".to_string(),
            })
        })?;

    // Transform text based on text-transform
    let transformed_text = apply_text_transform(text, styles.text_transform);

    // Handle white-space
    let processed_text = process_whitespace(&transformed_text, styles.white_space);

    if processed_text.is_empty() {
        return Ok(TextLayout {
            lines: Vec::new(),
            total_width: 0.0,
            total_height: 0.0,
        });
    }

    // Parse font with ttf-parser
    let ttf_face = ttf_parser::Face::parse(&font_face.data, font_face.index).map_err(|e| {
        Error::Font(crate::error::FontError::InvalidFontFile {
            path: format!("font index {}", font_face.index),
        })
    })?;

    // Get font metrics
    let units_per_em = ttf_face.units_per_em() as f32;
    let scale = styles.font_size / units_per_em;

    let ascender = ttf_face.ascender() as f32 * scale;
    let descender = ttf_face.descender() as f32 * scale;
    let line_gap = ttf_face.line_gap() as f32 * scale;

    let line_height_px = calculate_line_height(
        styles.line_height.clone(),
        styles.font_size,
        ascender,
        descender,
        line_gap,
    );

    // Shape text with rustybuzz
    let rb_face = Face::from_slice(&font_face.data, font_face.index).ok_or_else(|| {
        Error::Font(crate::error::FontError::InvalidFontFile {
            path: format!("font index {}", font_face.index),
        })
    })?;

    // Break into lines if max_width is specified and white-space allows wrapping
    let lines = if let Some(max_w) = max_width {
        // Don't break lines if white-space is nowrap or pre
        if styles.white_space == WhiteSpace::Nowrap || styles.white_space == WhiteSpace::Pre {
            // Single line, no wrapping
            let shaped_line = shape_line(&processed_text, &rb_face, &ttf_face, styles, &font_face, scale)?;
            vec![shaped_line]
        } else {
            break_text_into_lines(&processed_text, &rb_face, &ttf_face, styles, &font_face, scale, max_w)
        }
    } else {
        // Single line
        let shaped_line = shape_line(&processed_text, &rb_face, &ttf_face, styles, &font_face, scale)?;
        vec![shaped_line]
    };

    let total_width = lines.iter().map(|l| l.width).fold(0.0f32, f32::max);
    let total_height = lines.len() as f32 * line_height_px;

    Ok(TextLayout {
        lines,
        total_width,
        total_height,
    })
}

fn apply_text_transform(text: &str, transform: TextTransform) -> String {
    match transform {
        TextTransform::None => text.to_string(),
        TextTransform::Uppercase => text.to_uppercase(),
        TextTransform::Lowercase => text.to_lowercase(),
        TextTransform::Capitalize => {
            let mut result = String::new();
            let mut capitalize_next = true;
            for ch in text.chars() {
                if ch.is_whitespace() {
                    capitalize_next = true;
                    result.push(ch);
                } else if capitalize_next {
                    result.extend(ch.to_uppercase());
                    capitalize_next = false;
                } else {
                    result.push(ch);
                }
            }
            result
        }
    }
}

fn process_whitespace(text: &str, white_space: WhiteSpace) -> String {
    match white_space {
        WhiteSpace::Normal => {
            // Collapse whitespace
            text.split_whitespace().collect::<Vec<_>>().join(" ")
        }
        WhiteSpace::Nowrap => {
            // Collapse whitespace but don't wrap
            text.split_whitespace().collect::<Vec<_>>().join(" ")
        }
        WhiteSpace::Pre => {
            // Preserve all whitespace
            text.to_string()
        }
        WhiteSpace::PreWrap => {
            // Preserve whitespace but allow wrapping
            text.to_string()
        }
        WhiteSpace::PreLine => {
            // Collapse whitespace except newlines
            text.lines()
                .map(|line| line.split_whitespace().collect::<Vec<_>>().join(" "))
                .collect::<Vec<_>>()
                .join("\n")
        }
    }
}

fn calculate_line_height(line_height: LineHeight, font_size: f32, ascender: f32, descender: f32, line_gap: f32) -> f32 {
    match line_height {
        LineHeight::Normal => ascender - descender + line_gap,
        LineHeight::Number(n) => font_size * n,
        LineHeight::Length(len) => {
            if len.unit.is_absolute() {
                len.to_px()
            } else {
                len.resolve_with_font_size(font_size)
            }
        }
    }
}

fn break_text_into_lines(
    text: &str,
    rb_face: &Face,
    ttf_face: &ttf_parser::Face,
    styles: &ComputedStyles,
    font_face: &Arc<FontFace>,
    scale: f32,
    max_width: f32,
) -> Vec<TextLine> {
    let mut lines = Vec::new();
    let mut current_line = String::new();

    // Use Unicode line breaking algorithm
    let break_opportunities: Vec<(usize, BreakOpportunity)> = linebreaks(text).collect();

    let mut last_break = 0;
    for (break_pos, _opportunity) in break_opportunities {
        let segment = &text[last_break..break_pos];
        let test_line = if current_line.is_empty() {
            segment.to_string()
        } else {
            format!("{}{}", current_line, segment)
        };

        let test_width = measure_text_width(&test_line, rb_face, ttf_face, scale, styles);

        if test_width > max_width && !current_line.is_empty() {
            // Line would be too long, break here
            if let Ok(shaped_line) = shape_line(&current_line, rb_face, ttf_face, styles, font_face, scale) {
                lines.push(shaped_line);
            }
            current_line = segment.to_string();
        } else {
            current_line = test_line;
        }

        last_break = break_pos;
    }

    // Add remaining text
    if !current_line.is_empty() {
        if let Ok(shaped_line) = shape_line(&current_line, rb_face, ttf_face, styles, font_face, scale) {
            lines.push(shaped_line);
        }
    }

    lines
}

fn measure_text_width(
    text: &str,
    rb_face: &Face,
    _ttf_face: &ttf_parser::Face,
    scale: f32,
    styles: &ComputedStyles,
) -> f32 {
    let mut buffer = UnicodeBuffer::new();
    buffer.push_str(text);
    buffer.set_direction(Direction::LeftToRight);

    let output = rustybuzz::shape(rb_face, &[], buffer);
    let positions = output.glyph_positions();

    let mut width = 0.0;
    for pos in positions {
        width += pos.x_advance as f32 * scale;
    }

    // Add letter spacing
    width += (text.chars().count() as f32 - 1.0).max(0.0) * styles.letter_spacing;

    width
}

fn shape_line(
    text: &str,
    rb_face: &Face,
    ttf_face: &ttf_parser::Face,
    styles: &ComputedStyles,
    font_face: &Arc<FontFace>,
    scale: f32,
) -> Result<TextLine> {
    let mut buffer = UnicodeBuffer::new();
    buffer.push_str(text);
    buffer.set_direction(Direction::LeftToRight);

    let output = rustybuzz::shape(rb_face, &[], buffer);
    let glyph_infos = output.glyph_infos();
    let glyph_positions = output.glyph_positions();

    let ascender = ttf_face.ascender() as f32 * scale;
    let descender = ttf_face.descender() as f32 * scale;
    let line_gap = ttf_face.line_gap() as f32 * scale;

    let line_height = calculate_line_height(
        styles.line_height.clone(),
        styles.font_size,
        ascender,
        descender,
        line_gap,
    );
    let baseline = ascender;

    let mut glyphs = Vec::new();
    let mut x = 0.0;

    for (info, pos) in glyph_infos.iter().zip(glyph_positions.iter()) {
        let glyph_id = info.glyph_id as u16;

        // DEBUG: Log glyph shaping for vote arrows
        if text.contains("^") {
            eprintln!(
                "DEBUG: Shaping '{}' char at cluster {} -> glyph_id={}",
                text, info.cluster, glyph_id
            );
        }

        let x_offset = pos.x_offset as f32 * scale;
        let y_offset = pos.y_offset as f32 * scale;
        let x_advance = pos.x_advance as f32 * scale;

        // Get glyph bounding box for width/height
        let (glyph_width, glyph_height) = if let Some(bbox) = ttf_face.glyph_bounding_box(GlyphId(glyph_id)) {
            (
                (bbox.x_max - bbox.x_min) as f32 * scale,
                (bbox.y_max - bbox.y_min) as f32 * scale,
            )
        } else {
            (x_advance, line_height)
        };

        glyphs.push(ShapedGlyph {
            glyph_id,
            x: x + x_offset,
            y: y_offset,
            width: glyph_width,
            height: glyph_height,
            font_face: Arc::clone(font_face),
        });

        x += x_advance + styles.letter_spacing;
    }

    let width = x;

    Ok(TextLine {
        glyphs,
        width,
        height: line_height,
        baseline,
        x_offset: 0.0,
    })
}
