//! Display list renderer
//!
//! Executes a `DisplayList` into a pixel buffer using the existing canvas/text
//! rasterization utilities. This path keeps the display list as the
//! paint-time contract while still reusing the shared `FontContext`.

use crate::error::{RenderError, Result};
use crate::paint::canvas::Canvas;
use crate::paint::display_list::{
    ClipItem, DisplayItem, DisplayList, FillRectItem, FontId, ImageItem, OpacityItem, StrokeRectItem, TextItem,
    TransformItem,
};
use crate::style::color::Rgba;
use crate::text::font_db::{FontStyle as DbFontStyle, LoadedFont};
use crate::text::font_loader::FontContext;
use crate::text::shaper::GlyphPosition;
use tiny_skia::{BlendMode as SkiaBlendMode, Pixmap, Transform};

/// Renders a display list into a pixmap using the provided font context.
pub struct DisplayListRenderer {
    canvas: Canvas,
    font_ctx: FontContext,
}

impl DisplayListRenderer {
    /// Creates a renderer with the given dimensions and background color.
    pub fn new(width: u32, height: u32, background: Rgba, font_ctx: FontContext) -> Result<Self> {
        Ok(Self {
            canvas: Canvas::new(width, height, background)?,
            font_ctx,
        })
    }

    /// Consumes the renderer and returns the painted pixmap.
    pub fn render(mut self, list: &DisplayList) -> Result<Pixmap> {
        for item in list.items() {
            self.render_item(item)?;
        }
        Ok(self.canvas.into_pixmap())
    }

    fn render_item(&mut self, item: &DisplayItem) -> Result<()> {
        match item {
            DisplayItem::FillRect(FillRectItem { rect, color }) => self.canvas.draw_rect(*rect, *color),
            DisplayItem::StrokeRect(StrokeRectItem { rect, color, width }) => {
                self.canvas.stroke_rect(*rect, *color, *width)
            }
            DisplayItem::FillRoundedRect(item) => self.canvas.draw_rounded_rect(item.rect, item.radii, item.color),
            DisplayItem::StrokeRoundedRect(item) => self
                .canvas
                .stroke_rounded_rect(item.rect, item.radii, item.color, item.width),
            DisplayItem::Text(item) => self.render_text(item)?,
            DisplayItem::Image(item) => self.render_image(item)?,
            DisplayItem::BoxShadow(_)
            | DisplayItem::LinearGradient(_)
            | DisplayItem::RadialGradient(_)
            | DisplayItem::PushStackingContext(_)
            | DisplayItem::PopStackingContext => {
                // TODO: Implement full support for these items (stacking context layers, shadows, gradients)
            }
            DisplayItem::PushClip(clip) => self.push_clip(clip),
            DisplayItem::PopClip => self.pop_clip(),
            DisplayItem::PushOpacity(OpacityItem { opacity }) => {
                self.canvas.save();
                let combined = (self.canvas.opacity() * *opacity).clamp(0.0, 1.0);
                self.canvas.set_opacity(combined);
            }
            DisplayItem::PopOpacity => {
                self.canvas.restore();
            }
            DisplayItem::PushTransform(transform) => self.push_transform(transform),
            DisplayItem::PopTransform => {
                self.canvas.restore();
            }
            DisplayItem::PushBlendMode(mode) => {
                self.canvas.save();
                self.canvas.set_blend_mode(mode.mode);
            }
            DisplayItem::PopBlendMode => {
                self.canvas.restore();
            }
        }

        Ok(())
    }

    fn render_text(&mut self, item: &TextItem) -> Result<()> {
        let Some(font) = self.resolve_font(item.font_id.as_ref()) else {
            return Err(RenderError::RasterizationFailed {
                reason: "Unable to resolve font for display list text".into(),
            }
            .into());
        };

        let glyphs: Vec<GlyphPosition> = item
            .glyphs
            .iter()
            .map(|g| GlyphPosition {
                glyph_id: g.glyph_id,
                cluster: 0,
                advance: g.advance,
                advance_y: 0.0,
                offset_x: g.offset.x,
                offset_y: g.offset.y,
            })
            .collect();

        self.canvas
            .draw_text(item.origin, &glyphs, &font, item.font_size, item.color);
        Ok(())
    }

    fn render_image(&mut self, item: &ImageItem) -> Result<()> {
        if let Some(clip) = self.canvas.clip_bounds() {
            if clip.width() <= 0.0 || clip.height() <= 0.0 || clip.intersection(item.dest_rect).is_none() {
                return Ok(());
            }
        }

        let Some(pixmap) = self.image_to_pixmap(item) else {
            return Ok(());
        };

        if item.dest_rect.width() <= 0.0 || item.dest_rect.height() <= 0.0 {
            return Ok(());
        }

        let paint = tiny_skia::PixmapPaint {
            opacity: self.canvas.opacity(),
            blend_mode: SkiaBlendMode::SourceOver,
            ..Default::default()
        };

        let scale_x = item.dest_rect.width() / pixmap.width() as f32;
        let scale_y = item.dest_rect.height() / pixmap.height() as f32;

        let transform = Transform::from_row(scale_x, 0.0, 0.0, scale_y, item.dest_rect.x(), item.dest_rect.y())
            .post_concat(self.canvas.transform());
        let clip_mask = self.canvas.clip_mask().cloned();
        self.canvas
            .pixmap_mut()
            .draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, clip_mask.as_ref());

        Ok(())
    }

    fn push_clip(&mut self, clip: &ClipItem) {
        self.canvas.save();
        self.canvas.set_clip_with_radii(clip.rect, clip.radii);
    }

    fn pop_clip(&mut self) {
        self.canvas.restore();
    }

    fn push_transform(&mut self, transform: &TransformItem) {
        self.canvas.save();
        let matrix = Transform::from_row(
            transform.transform.a,
            transform.transform.b,
            transform.transform.c,
            transform.transform.d,
            transform.transform.e,
            transform.transform.f,
        );
        let combined = self.canvas.transform().post_concat(matrix);
        self.canvas.set_transform(combined);
    }

    fn resolve_font(&self, font_id: Option<&FontId>) -> Option<LoadedFont> {
        let mut families = Vec::new();
        let mut weight = 400;
        let mut italic = false;
        let mut oblique = false;

        if let Some(id) = font_id {
            families.push(id.family.clone());
            weight = id.weight;
            italic = matches!(id.style, DbFontStyle::Italic);
            oblique = matches!(id.style, DbFontStyle::Oblique);
        }

        if families.is_empty() {
            families.push("sans-serif".to_string());
        }

        self.font_ctx
            .get_font(&families, weight, italic, oblique)
            .or_else(|| self.font_ctx.get_sans_serif())
    }

    fn image_to_pixmap(&self, item: &ImageItem) -> Option<Pixmap> {
        let mut data = Vec::with_capacity((item.image.width * item.image.height * 4) as usize);
        let pixels = item.image.pixels.as_ref();
        for chunk in pixels.chunks_exact(4) {
            let r = chunk[0] as f32;
            let g = chunk[1] as f32;
            let b = chunk[2] as f32;
            let a = chunk[3] as f32 / 255.0;
            data.push((r * a).round() as u8);
            data.push((g * a).round() as u8);
            data.push((b * a).round() as u8);
            data.push(chunk[3]);
        }

        let size = tiny_skia::IntSize::from_wh(item.image.width, item.image.height)?;
        Pixmap::from_vec(data, size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::geometry::Rect;
    use crate::paint::display_list::{DisplayItem, DisplayList, FillRectItem, Transform2D};
    use crate::style::color::Rgba;

    fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
        let idx = ((y * pixmap.width() + x) * 4) as usize;
        let data = pixmap.data();
        (data[idx], data[idx + 1], data[idx + 2], data[idx + 3])
    }

    #[test]
    fn transform_push_is_scoped() {
        let renderer = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::PushTransform(TransformItem {
            transform: Transform2D {
                a: 1.0,
                b: 0.0,
                c: 0.0,
                d: 1.0,
                e: 10.0,
                f: 0.0,
            },
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::PopTransform);
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
            color: Rgba::rgb(0, 255, 0),
        }));

        let pixmap = renderer.render(&list).unwrap();
        assert_eq!(pixel(&pixmap, 10, 0), (255, 0, 0, 255));
        assert_eq!(pixel(&pixmap, 0, 0), (0, 255, 0, 255));
    }
}
