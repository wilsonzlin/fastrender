use fastrender::paint::text_rasterize::glyph_advance;
use fastrender::text::color_fonts::ColorFontRenderer;
use fastrender::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
use fastrender::text::font_instance::FontInstance;
use fastrender::text::pipeline::GlyphPosition;
use fastrender::{Rgba, TextRasterizer};
use rustybuzz::Variation;
use std::sync::Arc;
use tiny_skia::{Color, Pixmap};

#[test]
fn renders_sbix_jpeg_bitmap_glyph() {
  let font = LoadedFont {
    id: None,
    data: Arc::new(
      std::fs::read("tests/fixtures/fonts/TestSbixJPEG.ttf").expect("fixture font should exist"),
    ),
    index: 0,
    family: "Test Sbix JPEG".to_string(),
    weight: FontWeight::NORMAL,
    style: FontStyle::Normal,
    stretch: FontStretch::Normal,
  };
  let face = font.as_ttf_face().expect("fixture font should parse");
  let glyph_id = face.glyph_index('A').expect("fixture should map A").0 as u32;

  let font_size = 32.0;
  let x_advance = glyph_advance(&font, glyph_id, font_size).unwrap_or(font_size);
  let glyphs = vec![GlyphPosition {
    glyph_id,
    cluster: 0,
    x_offset: 0.0,
    y_offset: 0.0,
    x_advance,
    y_advance: 0.0,
  }];

  let mut color_renderer = ColorFontRenderer::new();
  let variations: Vec<Variation> = Vec::new();
  let instance = FontInstance::new(&font, &variations).expect("font instance");
  let color_glyph = color_renderer
    .render(
      &font,
      &instance,
      glyph_id,
      font_size,
      0,
      &[],
      0,
      Rgba::BLUE,
      0.0,
      &variations,
      None,
    )
    .expect("color renderer should produce a bitmap glyph for the sbix strike");

  let draw_x = (8.0 + color_glyph.left).round() as i32;
  let draw_y = (48.0 + color_glyph.top).round() as i32;

  let mut glyph_red = 0;
  let mut glyph_green = 0;
  for chunk in color_glyph.image.data().chunks(4) {
    let b = chunk[0];
    let g = chunk[1];
    let r = chunk[2];
    let a = chunk[3];
    if a == 0 {
      continue;
    }
    if r > g.saturating_add(40) && r > b.saturating_add(40) && r > 180 {
      glyph_red += 1;
    }
    if g > r.saturating_add(40) && g > b.saturating_add(40) && g > 150 {
      glyph_green += 1;
    }
  }
  assert!(
    glyph_red > 0 && glyph_green > 0,
    "decoded glyph bitmap should contain both red and green regions (red={}, green={}, size={}x{})",
    glyph_red,
    glyph_green,
    color_glyph.image.width(),
    color_glyph.image.height()
  );

  let mut rasterizer = TextRasterizer::new();
  let mut pixmap = Pixmap::new(64, 64).expect("pixmap allocation should succeed");
  pixmap.fill(Color::WHITE);

  rasterizer
    .render_glyphs(
      &glyphs,
      &font,
      font_size,
      8.0,
      48.0,
      Rgba::BLUE,
      &mut pixmap,
    )
    .expect("rendering should succeed");

  let mut max_r = 0u8;
  let mut max_g = 0u8;
  let mut max_b = 0u8;
  let mut painted_pixels = 0usize;
  let mut red_pixels = 0;
  let mut green_pixels = 0;

  for y in 0..pixmap.height() {
    for x in 0..pixmap.width() {
      let pixel = pixmap.pixel(x, y).unwrap();
      let r = pixel.red();
      let g = pixel.green();
      let b = pixel.blue();
      let a = pixel.alpha();

      if a == 0 {
        continue;
      }

      if r != 255 || g != 255 || b != 255 || a != 255 {
        painted_pixels += 1;
      }

      max_r = max_r.max(r);
      max_g = max_g.max(g);
      max_b = max_b.max(b);

      if r > g.saturating_add(40) && r > b.saturating_add(40) && r > 180 {
        red_pixels += 1;
      }

      if g > r.saturating_add(40) && g > b.saturating_add(40) && g > 150 {
        green_pixels += 1;
      }
    }
  }

  assert!(
    red_pixels > 0 && green_pixels > 0,
    "sbix JPEG bitmap should survive decoding and preserve embedded colors (red={}, green={}, painted={}, max_channels=({}, {}, {}), draw=({}, {}), glyph_size={}x{})",
    red_pixels,
    green_pixels,
    painted_pixels,
    max_r,
    max_g,
    max_b,
    draw_x,
    draw_y,
    color_glyph.image.width(),
    color_glyph.image.height()
  );
}
