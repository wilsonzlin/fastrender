use crate::style::color::Rgba;
use crate::style::values::{Length, LengthUnit};
use crate::style::ComputedStyle;

#[derive(Debug, Clone)]
pub struct ResolvedTextShadow {
    pub offset_x: f32,
    pub offset_y: f32,
    pub blur_radius: f32,
    pub color: Rgba,
}

#[derive(Debug, Clone, Copy)]
pub struct PathBounds {
    pub min_x: f32,
    pub min_y: f32,
    pub max_x: f32,
    pub max_y: f32,
}

impl PathBounds {
    pub fn new() -> Self {
        Self {
            min_x: f32::INFINITY,
            min_y: f32::INFINITY,
            max_x: f32::NEG_INFINITY,
            max_y: f32::NEG_INFINITY,
        }
    }

    pub fn include(&mut self, rect: &tiny_skia::Rect) {
        self.min_x = self.min_x.min(rect.left());
        self.min_y = self.min_y.min(rect.top());
        self.max_x = self.max_x.max(rect.right());
        self.max_y = self.max_y.max(rect.bottom());
    }

    pub fn is_valid(&self) -> bool {
        self.min_x.is_finite() && self.min_y.is_finite() && self.max_x.is_finite() && self.max_y.is_finite()
    }
}

pub fn resolve_text_shadows(style: &ComputedStyle) -> Vec<ResolvedTextShadow> {
    style
        .text_shadow
        .iter()
        .map(|shadow| ResolvedTextShadow {
            offset_x: resolve_shadow_length(&shadow.offset_x, style.font_size),
            offset_y: resolve_shadow_length(&shadow.offset_y, style.font_size),
            blur_radius: resolve_shadow_length(&shadow.blur_radius, style.font_size).max(0.0),
            color: shadow.color.unwrap_or(style.color),
        })
        .collect()
}

fn resolve_shadow_length(len: &Length, font_size: f32) -> f32 {
    match len.unit {
        LengthUnit::Percent => len.resolve_against(font_size).unwrap_or(0.0),
        LengthUnit::Em | LengthUnit::Rem => len.resolve_with_font_size(font_size).unwrap_or(len.value * font_size),
        _ if len.unit.is_absolute() => len.to_px(),
        _ => len.value * font_size,
    }
}
