//! Display list renderer
//!
//! Executes a `DisplayList` into a pixel buffer using the existing canvas/text
//! rasterization utilities. This path keeps the display list as the
//! paint-time contract while still reusing the shared `FontContext`.

use crate::error::{RenderError, Result};
use crate::paint::canvas::Canvas;
use crate::paint::display_list::{
    BoxShadowItem, ClipItem, DisplayItem, DisplayList, FillRectItem, FontId, ImageItem, LinearGradientItem,
    OpacityItem, RadialGradientItem, StrokeRectItem, TextItem, TransformItem,
};
use crate::paint::rasterize::{render_box_shadow, BoxShadow};
use crate::style::color::Rgba;
use crate::text::font_db::{FontStyle as DbFontStyle, LoadedFont};
use crate::text::font_loader::FontContext;
use crate::text::shaper::GlyphPosition;
use tiny_skia::{
    GradientStop as SkiaGradientStop, LinearGradient, Pixmap, Point as SkiaPoint, RadialGradient, SpreadMode, Transform,
};

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

    fn render_linear_gradient(&mut self, item: &LinearGradientItem) {
        let Some(stops) = self.convert_stops(&item.stops) else {
            return;
        };
        let start = SkiaPoint::from_xy(item.rect.x() + item.start.x, item.rect.y() + item.start.y);
        let end = SkiaPoint::from_xy(item.rect.x() + item.end.x, item.rect.y() + item.end.y);
        let spread = match item.spread {
            crate::paint::display_list::GradientSpread::Pad => SpreadMode::Pad,
            crate::paint::display_list::GradientSpread::Repeat => SpreadMode::Repeat,
            crate::paint::display_list::GradientSpread::Reflect => SpreadMode::Reflect,
        };
        let Some(shader) = LinearGradient::new(start, end, stops, spread, Transform::identity()) else {
            return;
        };

        let Some(skia_rect) =
            tiny_skia::Rect::from_xywh(item.rect.x(), item.rect.y(), item.rect.width(), item.rect.height())
        else {
            return;
        };

        let path = tiny_skia::PathBuilder::from_rect(skia_rect);

        let mut paint = tiny_skia::Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        paint.blend_mode = self.canvas.blend_mode();
        let transform = self.canvas.transform();
        let clip = self.canvas.clip_mask().cloned();
        self.canvas
            .pixmap_mut()
            .fill_path(&path, &paint, tiny_skia::FillRule::Winding, transform, clip.as_ref());
    }

    fn render_radial_gradient(&mut self, item: &RadialGradientItem) {
        let Some(stops) = self.convert_stops(&item.stops) else {
            return;
        };
        let center = SkiaPoint::from_xy(item.rect.x() + item.center.x, item.rect.y() + item.center.y);
        let spread = match item.spread {
            crate::paint::display_list::GradientSpread::Pad => SpreadMode::Pad,
            crate::paint::display_list::GradientSpread::Repeat => SpreadMode::Repeat,
            crate::paint::display_list::GradientSpread::Reflect => SpreadMode::Reflect,
        };
        let Some(shader) = RadialGradient::new(
            center,
            center,
            item.radius,
            stops,
            spread,
            Transform::identity(),
        ) else {
            return;
        };

        let Some(skia_rect) =
            tiny_skia::Rect::from_xywh(item.rect.x(), item.rect.y(), item.rect.width(), item.rect.height())
        else {
            return;
        };

        let path = tiny_skia::PathBuilder::from_rect(skia_rect);

        let mut paint = tiny_skia::Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        paint.blend_mode = self.canvas.blend_mode();
        let transform = self.canvas.transform();
        let clip = self.canvas.clip_mask().cloned();
        self.canvas
            .pixmap_mut()
            .fill_path(&path, &paint, tiny_skia::FillRule::Winding, transform, clip.as_ref());
    }

    fn convert_stops(
        &self,
        stops: &[crate::paint::display_list::GradientStop],
    ) -> Option<Vec<tiny_skia::GradientStop>> {
        if stops.is_empty() {
            return None;
        }
        let opacity = self.canvas.opacity().clamp(0.0, 1.0);
        Some(
            stops
                .iter()
                .map(|s| {
                    let alpha = (s.color.a * opacity * 255.0).round().clamp(0.0, 255.0) as u8;
                    SkiaGradientStop::new(
                        s.position,
                        tiny_skia::Color::from_rgba8(s.color.r, s.color.g, s.color.b, alpha),
                    )
                })
                .collect(),
        )
    }

    fn render_box_shadow(&mut self, item: &BoxShadowItem) {
        let mut shadow = BoxShadow {
            offset_x: item.offset.x,
            offset_y: item.offset.y,
            blur_radius: item.blur_radius,
            spread_radius: item.spread_radius,
            color: item.color,
            inset: item.inset,
        };

        let mut temp = match Pixmap::new(self.canvas.width(), self.canvas.height()) {
            Some(p) => p,
            None => return,
        };

        // Apply current opacity to the shadow when compositing.
        let opacity = self.canvas.opacity().clamp(0.0, 1.0);
        shadow.color.a *= opacity;

        let _ = render_box_shadow(
            &mut temp,
            item.rect.x(),
            item.rect.y(),
            item.rect.width(),
            item.rect.height(),
            &item.radii,
            &shadow,
        );

        let paint = tiny_skia::PixmapPaint {
            opacity: 1.0,
            blend_mode: self.canvas.blend_mode(),
            ..Default::default()
        };
        let clip = self.canvas.clip_mask().cloned();
        let transform = self.canvas.transform();
        self.canvas
            .pixmap_mut()
            .draw_pixmap(0, 0, temp.as_ref(), &paint, transform, clip.as_ref());
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
            DisplayItem::LinearGradient(item) => self.render_linear_gradient(item),
            DisplayItem::RadialGradient(item) => self.render_radial_gradient(item),
            DisplayItem::Text(item) => self.render_text(item)?,
            DisplayItem::Image(item) => self.render_image(item)?,
            DisplayItem::BoxShadow(item) => self.render_box_shadow(item),
            DisplayItem::PushStackingContext(_) => self.canvas.save(),
            DisplayItem::PopStackingContext => self.canvas.restore(),
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
            blend_mode: self.canvas.blend_mode(),
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
    use crate::geometry::{Point, Rect};
    use crate::paint::display_list::{
        BoxShadowItem, DisplayItem, DisplayList, FillRectItem, GradientSpread, GradientStop, LinearGradientItem,
        RadialGradientItem, Transform2D,
    };
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

    #[test]
    fn renders_linear_gradient() {
        let renderer = DisplayListRenderer::new(2, 1, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::LinearGradient(LinearGradientItem {
            rect: Rect::from_xywh(0.0, 0.0, 2.0, 1.0),
            start: Point::new(0.0, 0.0),
            end: Point::new(2.0, 0.0),
            spread: GradientSpread::Pad,
            stops: vec![
                GradientStop {
                    position: 0.0,
                    color: Rgba::rgb(255, 0, 0),
                },
                GradientStop {
                    position: 1.0,
                    color: Rgba::rgb(0, 0, 255),
                },
            ],
        }));

        let pixmap = renderer.render(&list).unwrap();
        let first = pixel(&pixmap, 0, 0);
        let second = pixel(&pixmap, 1, 0);
        assert!(first.0 > first.2);
        assert!(second.2 > second.0);
        assert_eq!(first.3, 255);
        assert_eq!(second.3, 255);
    }

    #[test]
    fn renders_repeating_linear_gradient() {
        let renderer = DisplayListRenderer::new(4, 1, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::LinearGradient(LinearGradientItem {
            rect: Rect::from_xywh(0.0, 0.0, 4.0, 1.0),
            start: Point::new(0.0, 0.0),
            end: Point::new(1.0, 0.0),
            spread: GradientSpread::Repeat,
            stops: vec![
                GradientStop {
                    position: 0.0,
                    color: Rgba::rgb(255, 0, 0),
                },
                GradientStop {
                    position: 1.0,
                    color: Rgba::rgb(0, 0, 255),
                },
            ],
        }));

        let pixmap = renderer.render(&list).unwrap();
        let right = pixel(&pixmap, 3, 0);
        assert!(right.0 > 0, "repeat spread should reintroduce the start color");
        assert!(right.2 > 0);
        assert_eq!(right.3, 255);
    }

    #[test]
    fn renders_radial_gradient_center_color() {
        let renderer = DisplayListRenderer::new(3, 3, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::RadialGradient(RadialGradientItem {
            rect: Rect::from_xywh(0.0, 0.0, 3.0, 3.0),
            center: Point::new(1.5, 1.5),
            radius: 2.0,
            spread: GradientSpread::Pad,
            stops: vec![
                GradientStop {
                    position: 0.0,
                    color: Rgba::rgb(0, 255, 0),
                },
                GradientStop {
                    position: 1.0,
                    color: Rgba::rgb(0, 0, 0),
                },
            ],
        }));

        let pixmap = renderer.render(&list).unwrap();
        assert_eq!(pixel(&pixmap, 1, 1).1, 255);
    }

    #[test]
    fn renders_box_shadow() {
        let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        list.push(DisplayItem::BoxShadow(BoxShadowItem {
            rect: Rect::from_xywh(2.0, 2.0, 4.0, 4.0),
            radii: crate::paint::display_list::BorderRadii::uniform(0.0),
            offset: Point::new(0.0, 0.0),
            blur_radius: 0.0,
            spread_radius: 0.0,
            color: Rgba::new(0, 0, 0, 0.5),
            inset: false,
        }));

        let pixmap = renderer.render(&list).unwrap();
        // Center of the box should have darkened pixels due to shadow.
        assert!(pixel(&pixmap, 3, 3).3 > 200);
    }

    #[test]
    fn stacking_context_restores_clip() {
        let renderer = DisplayListRenderer::new(6, 6, Rgba::WHITE, FontContext::new()).unwrap();
        let mut list = DisplayList::new();
        // Outer clip to a 4x4 square.
        list.push(DisplayItem::PushClip(ClipItem {
            rect: Rect::from_xywh(1.0, 1.0, 4.0, 4.0),
            radii: None,
        }));
        // Start stacking context and narrow the clip further.
        list.push(DisplayItem::PushStackingContext(
            crate::paint::display_list::StackingContextItem {
                z_index: 0,
                creates_stacking_context: true,
                bounds: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
            },
        ));
        list.push(DisplayItem::PushClip(ClipItem {
            rect: Rect::from_xywh(2.0, 2.0, 2.0, 2.0),
            radii: None,
        }));
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
            color: Rgba::rgb(255, 0, 0),
        }));
        list.push(DisplayItem::PopClip);
        list.push(DisplayItem::PopStackingContext);
        // After popping stacking context, outer clip should remain.
        list.push(DisplayItem::FillRect(FillRectItem {
            rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
            color: Rgba::rgb(0, 0, 255),
        }));
        list.push(DisplayItem::PopClip);

        let pixmap = renderer.render(&list).unwrap();
        // Pixel inside outer clip but outside inner clip should be blue (second fill) not red.
        assert_eq!(pixel(&pixmap, 1, 1), (0, 0, 255, 255));
    }
}
