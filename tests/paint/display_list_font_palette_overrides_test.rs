use super::color_font_helpers::load_test_font;
use fastrender::css::types::{FontPaletteBase, FontPaletteOverride, FontPaletteValuesRule, TextShadow};
use fastrender::geometry::Rect;
use fastrender::paint::display_list::DisplayItem;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::paint::text_rasterize::{TextRasterizer, TextRenderState};
use fastrender::paint::text_shadow::{resolve_text_shadows, PathBounds};
use fastrender::style::color::{Color, Rgba};
use fastrender::style::font_palette::FontPaletteRegistry;
use fastrender::style::types::FontPalette;
use fastrender::style::values::Length;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::{ShapedRun, ShapingPipeline};
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::ComputedStyle;
use std::collections::HashMap;
use std::sync::Arc;
use tiny_skia::{Color as SkColor, Pixmap, PixmapPaint, PremultipliedColorU8, Transform};

fn load_colr_test_font_context() -> (FontContext, String) {
  let font = load_test_font("ColorTestCOLR.ttf");
  let bytes = (*font.data).clone();
  let mut db = FontDatabase::empty();
  db.load_font_data(bytes)
    .expect("load ColorTestCOLR.ttf into database");
  let family = db
    .first_font()
    .expect("color test font should be present")
    .family
    .clone();
  (FontContext::with_database(Arc::new(db)), family)
}

fn build_palette_registry(family: &str) -> Arc<FontPaletteRegistry> {
  let mut registry = FontPaletteRegistry::default();

  let mut green = FontPaletteValuesRule::new("--green");
  green.font_families.push(family.to_string());
  green.base_palette = FontPaletteBase::Index(0);
  green.overrides.push(FontPaletteOverride {
    index: 1,
    color: Color::Rgba(Rgba::GREEN),
  });
  registry.register(green);

  let mut no_blue = FontPaletteValuesRule::new("--no-blue");
  no_blue.font_families.push(family.to_string());
  no_blue.base_palette = FontPaletteBase::Index(0);
  no_blue.overrides.push(FontPaletteOverride {
    index: 0,
    color: Color::Rgba(Rgba::new(0, 0, 255, 0.0)),
  });
  no_blue.overrides.push(FontPaletteOverride {
    index: 1,
    color: Color::Rgba(Rgba::GREEN),
  });
  registry.register(no_blue);

  Arc::new(registry)
}

fn shape_single_run(text: &str, style: &ComputedStyle, font_ctx: &FontContext) -> ShapedRun {
  let pipeline = ShapingPipeline::new();
  let runs = pipeline
    .shape(text, style, font_ctx)
    .expect("shape runs");
  runs.into_iter().next().expect("expected a shaped run")
}

fn unpremultiply(channel: u8, alpha: u8) -> u8 {
  if alpha == 0 {
    return 0;
  }
  (((channel as u16) * 255 + (alpha as u16 / 2)) / alpha as u16).min(255) as u8
}

fn color_histogram(pixmap: &Pixmap) -> HashMap<[u8; 3], usize> {
  let mut hist = HashMap::new();
  for px in pixmap.data().chunks_exact(4) {
    let alpha = px[3];
    if alpha == 0 {
      continue;
    }
    let entry = hist
      .entry([
        unpremultiply(px[2], alpha),
        unpremultiply(px[1], alpha),
        unpremultiply(px[0], alpha),
      ])
      .or_insert(0);
    *entry += 1;
  }
  hist
}

#[inline]
fn mul_div_255_round_u16(a: u16, b: u16) -> u16 {
  let prod = (a as u32) * (b as u32);
  (((prod + 128) * 257) >> 16) as u16
}

fn tint_shadow_pixmap(pixmap: &mut Pixmap, color: Rgba) {
  let r = color.r as u16;
  let g = color.g as u16;
  let b = color.b as u16;

  for px in pixmap.pixels_mut() {
    let a = px.alpha();
    if a == 0 {
      *px = PremultipliedColorU8::TRANSPARENT;
      continue;
    }
    let a16 = a as u16;
    let rp = mul_div_255_round_u16(r, a16) as u8;
    let gp = mul_div_255_round_u16(g, a16) as u8;
    let bp = mul_div_255_round_u16(b, a16) as u8;
    *px = PremultipliedColorU8::from_rgba(rp, gp, bp, a)
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
}

fn shadow_bounds_for_run(run: &ShapedRun, origin_x: f32, baseline_y: f32) -> PathBounds {
  let mut bounds = PathBounds::new();

  if run.glyphs.is_empty() || !run.font_size.is_finite() || run.font_size <= 0.0 {
    return bounds;
  }

  let effective_font_size = run.font_size * run.scale;
  let ascent = effective_font_size;
  let descent = effective_font_size * 0.25;

  let mut cursor_x = origin_x;
  let mut cursor_y = 0.0_f32;
  for glyph in &run.glyphs {
    let glyph_x = cursor_x + glyph.x_offset;
    let glyph_y = baseline_y + cursor_y + glyph.y_offset;

    let x0 = glyph_x.min(glyph_x + glyph.x_advance);
    let x1 = glyph_x.max(glyph_x + glyph.x_advance);
    let y0 = glyph_y - ascent;
    let y1 = glyph_y + descent;
    for (x, y) in [(x0, y0), (x0, y1), (x1, y0), (x1, y1)] {
      bounds.min_x = bounds.min_x.min(x);
      bounds.max_x = bounds.max_x.max(x);
      bounds.min_y = bounds.min_y.min(y);
      bounds.max_y = bounds.max_y.max(y);
    }

    cursor_x += glyph.x_advance;
    cursor_y += glyph.y_advance;
  }

  if bounds.is_valid() {
    let bold_pad = if run.synthetic_bold.is_finite() {
      run.synthetic_bold.abs()
    } else {
      0.0
    };
    let skew = if run.synthetic_oblique.is_finite() {
      run.synthetic_oblique
    } else {
      0.0
    };
    let oblique_pad = skew.abs() * (ascent + descent);
    let pad_x = bold_pad + oblique_pad + 1.0;
    let pad_y = bold_pad + 1.0;
    bounds.min_x -= pad_x;
    bounds.max_x += pad_x;
    bounds.min_y -= pad_y;
    bounds.max_y += pad_y;
  }

  bounds
}

fn render_legacy_text_shadows(
  rasterizer: &mut TextRasterizer,
  run: &ShapedRun,
  origin_x: f32,
  baseline_y: f32,
  shadows: &[fastrender::paint::display_list::TextShadowItem],
  pixmap: &mut Pixmap,
) {
  if shadows.is_empty() || run.glyphs.is_empty() {
    return;
  }

  let bounds = shadow_bounds_for_run(run, origin_x, baseline_y);
  assert!(bounds.is_valid(), "shadow bounds should be valid");

  for shadow in shadows {
    let blur_margin = (shadow.blur_radius.abs() * 3.0).ceil();
    let shadow_min_x = bounds.min_x + shadow.offset.x - blur_margin;
    let shadow_max_x = bounds.max_x + shadow.offset.x + blur_margin;
    let shadow_min_y = bounds.min_y + shadow.offset.y - blur_margin;
    let shadow_max_y = bounds.max_y + shadow.offset.y + blur_margin;

    let shadow_width = (shadow_max_x - shadow_min_x).ceil().max(0.0) as u32;
    let shadow_height = (shadow_max_y - shadow_min_y).ceil().max(0.0) as u32;
    if shadow_width == 0 || shadow_height == 0 {
      continue;
    }

    let mut shadow_pixmap = Pixmap::new(shadow_width, shadow_height).expect("shadow pixmap");
    shadow_pixmap.fill(SkColor::TRANSPARENT);

    let translate_x = -bounds.min_x + blur_margin;
    let translate_y = -bounds.min_y + blur_margin;
    let state = TextRenderState {
      transform: Transform::from_translate(translate_x, translate_y),
      opacity: 1.0,
      blend_mode: tiny_skia::BlendMode::SourceOver,
      clip_mask: None,
    };

    rasterizer
      .render_glyph_run(
        &run.glyphs,
        &run.font,
        run.font_size * run.scale,
        run.synthetic_bold,
        run.synthetic_oblique,
        run.palette_index,
        &run.palette_overrides,
        run.palette_override_hash,
        &run.variations,
        None,
        origin_x,
        baseline_y,
        shadow.color,
        state,
        &mut shadow_pixmap,
      )
      .expect("render shadow glyph run");

    tint_shadow_pixmap(&mut shadow_pixmap, shadow.color);

    if shadow.blur_radius > 0.0 {
      fastrender::paint::blur::apply_gaussian_blur_anisotropic(
        &mut shadow_pixmap,
        shadow.blur_radius,
        shadow.blur_radius,
      )
      .expect("apply gaussian blur");
    }

    let dest_x = shadow_min_x.floor() as i32;
    let dest_y = shadow_min_y.floor() as i32;
    let frac_x = shadow_min_x - dest_x as f32;
    let frac_y = shadow_min_y - dest_y as f32;
    let paint = PixmapPaint {
      opacity: 1.0,
      blend_mode: tiny_skia::BlendMode::SourceOver,
      ..Default::default()
    };
    let transform = Transform::from_translate(frac_x, frac_y);
    pixmap.draw_pixmap(
      dest_x,
      dest_y,
      shadow_pixmap.as_ref(),
      &paint,
      transform,
      None,
    );
  }
}

#[test]
fn display_list_preserves_font_palette_overrides_for_text() {
  let (font_ctx, family) = load_colr_test_font_context();
  let palette_registry = build_palette_registry(&family);

  let mut style = ComputedStyle::default();
  style.font_family = vec![family.clone()].into();
  style.font_size = 64.0;
  style.root_font_size = style.font_size;
  style.font_palettes = palette_registry.clone();
  style.font_palette = FontPalette::Named("--green".into());
  let style = Arc::new(style);

  let run = shape_single_run("A", &style, &font_ctx);
  assert!(
    !run.palette_overrides.is_empty(),
    "expected non-empty palette overrides from @font-palette-values"
  );
  assert_ne!(
    run.palette_override_hash, 0,
    "expected non-zero palette override hash when overrides are present"
  );

  let width = 140;
  let height = 140;
  let start_x = 20.0;
  let baseline = 110.0;

  let fragment = FragmentNode::new_text_shaped(
    Rect::from_xywh(start_x, 0.0, 100.0, height as f32),
    "A",
    baseline,
    vec![run.clone()],
    style,
  );
  let list = DisplayListBuilder::new().build(&fragment);

  let dl_pixmap = DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("render display list");

  let mut legacy = Pixmap::new(width, height).unwrap();
  legacy.fill(SkColor::WHITE);
  TextRasterizer::new()
    .render_shaped_run(&run, start_x, baseline, Rgba::BLACK, &mut legacy)
    .expect("render legacy");

  assert_eq!(
    dl_pixmap.data(),
    legacy.data(),
    "display-list rendering should match legacy rasterizer output when palette overrides are active"
  );

  let hist = color_histogram(&dl_pixmap);
  assert!(hist.contains_key(&[0, 255, 0]), "expected green override pixels");
  assert!(
    !hist.contains_key(&[255, 0, 0]),
    "expected base red palette entry to be absent after overrides"
  );

  let text_item = list
    .items()
    .iter()
    .find_map(|item| match item {
      DisplayItem::Text(item) => Some(item),
      _ => None,
    })
    .expect("expected a text display item");
  assert_eq!(text_item.palette_override_hash, run.palette_override_hash);
  assert_eq!(text_item.palette_overrides.as_ref(), run.palette_overrides.as_ref());
}

#[test]
fn display_list_palette_override_hash_avoids_cache_aliasing_for_text() {
  let (font_ctx, family) = load_colr_test_font_context();
  let palette_registry = build_palette_registry(&family);

  let mut base_style = ComputedStyle::default();
  base_style.font_family = vec![family.clone()].into();
  base_style.font_size = 64.0;
  base_style.root_font_size = base_style.font_size;
  base_style.font_palettes = palette_registry.clone();

  let mut normal_style = base_style.clone();
  normal_style.font_palette = FontPalette::Normal;
  let normal_run = shape_single_run("A", &normal_style, &font_ctx);
  assert!(
    normal_run.palette_overrides.is_empty(),
    "normal palette should not produce overrides"
  );
  assert_eq!(normal_run.palette_override_hash, 0);

  let mut green_style = base_style.clone();
  green_style.font_palette = FontPalette::Named("--green".into());
  let green_run = shape_single_run("A", &green_style, &font_ctx);
  assert!(!green_run.palette_overrides.is_empty());
  assert_ne!(green_run.palette_override_hash, 0);

  let width = 260;
  let height = 140;
  let baseline = 110.0;
  let left_x = 20.0;
  let right_x = 140.0;

  let left = FragmentNode::new_text_shaped(
    Rect::from_xywh(left_x, 0.0, 100.0, height as f32),
    "A",
    baseline,
    vec![normal_run.clone()],
    Arc::new(normal_style),
  );
  let right = FragmentNode::new_text_shaped(
    Rect::from_xywh(right_x, 0.0, 100.0, height as f32),
    "A",
    baseline,
    vec![green_run.clone()],
    Arc::new(green_style),
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, width as f32, height as f32), vec![
    left, right,
  ]);

  let list = DisplayListBuilder::new().build(&root);
  let dl_pixmap = DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("render display list");

  let mut legacy = Pixmap::new(width, height).unwrap();
  legacy.fill(SkColor::WHITE);
  let mut rasterizer = TextRasterizer::new();
  rasterizer
    .render_shaped_run(&normal_run, left_x, baseline, Rgba::BLACK, &mut legacy)
    .unwrap();
  rasterizer
    .render_shaped_run(&green_run, right_x, baseline, Rgba::BLACK, &mut legacy)
    .unwrap();

  assert_eq!(
    dl_pixmap.data(),
    legacy.data(),
    "palette_override_hash must be preserved so cached color glyph rasters do not alias across different overrides"
  );
}

#[test]
fn display_list_preserves_palette_overrides_for_text_shadows() {
  let (font_ctx, family) = load_colr_test_font_context();
  let palette_registry = build_palette_registry(&family);

  let mut style_base = ComputedStyle::default();
  style_base.font_family = vec![family.clone()].into();
  style_base.font_size = 64.0;
  style_base.root_font_size = style_base.font_size;
  style_base.font_palettes = palette_registry.clone();
  style_base.color = Rgba::new(255, 255, 255, 0.0);
  style_base.text_shadow = vec![TextShadow {
    offset_x: Length::px(6.0),
    offset_y: Length::px(0.0),
    blur_radius: Length::px(0.0),
    color: Some(Rgba::BLACK),
  }]
  .into();

  let mut green_style = style_base.clone();
  green_style.font_palette = FontPalette::Named("--green".into());
  let green_run = shape_single_run("A", &green_style, &font_ctx);
  assert!(!green_run.palette_overrides.is_empty());

  let mut no_blue_style = style_base.clone();
  no_blue_style.font_palette = FontPalette::Named("--no-blue".into());
  let no_blue_run = shape_single_run("A", &no_blue_style, &font_ctx);
  assert!(
    no_blue_run
      .palette_overrides
      .iter()
      .any(|(idx, color)| *idx == 0 && color.alpha_u8() == 0),
    "expected palette entry 0 to be overridden with transparent color"
  );
  assert_ne!(no_blue_run.palette_override_hash, green_run.palette_override_hash);

  let width = 260;
  let height = 140;
  let baseline = 110.0;
  let left_x = 20.0;
  let right_x = 140.0;

  let left = FragmentNode::new_text_shaped(
    Rect::from_xywh(left_x, 0.0, 100.0, height as f32),
    "A",
    baseline,
    vec![green_run.clone()],
    Arc::new(green_style.clone()),
  );
  let right = FragmentNode::new_text_shaped(
    Rect::from_xywh(right_x, 0.0, 100.0, height as f32),
    "A",
    baseline,
    vec![no_blue_run.clone()],
    Arc::new(no_blue_style.clone()),
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, width as f32, height as f32), vec![
    left, right,
  ]);
  let list = DisplayListBuilder::new().build(&root);

  let dl_pixmap = DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("render display list with shadows");

  let mut legacy = Pixmap::new(width, height).unwrap();
  legacy.fill(SkColor::WHITE);
  let mut rasterizer = TextRasterizer::new();

  for (run, style, origin_x) in [
    (&green_run, &green_style, left_x),
    (&no_blue_run, &no_blue_style, right_x),
  ] {
    let shadows: Vec<fastrender::paint::display_list::TextShadowItem> = resolve_text_shadows(style)
      .into_iter()
      .map(|shadow| fastrender::paint::display_list::TextShadowItem {
        offset: fastrender::geometry::Point::new(shadow.offset_x, shadow.offset_y),
        blur_radius: shadow.blur_radius,
        color: shadow.color,
      })
      .collect();
    render_legacy_text_shadows(&mut rasterizer, run, origin_x, baseline, &shadows, &mut legacy);
  }

  assert_eq!(
    dl_pixmap.data(),
    legacy.data(),
    "display-list text-shadow rendering should match legacy shadow rasterization when palette overrides are active"
  );
}

#[test]
fn display_list_preserves_font_palette_overrides_for_list_markers() {
  let (font_ctx, family) = load_colr_test_font_context();
  let palette_registry = build_palette_registry(&family);

  let mut style = ComputedStyle::default();
  style.font_family = vec![family.clone()].into();
  style.font_size = 64.0;
  style.root_font_size = style.font_size;
  style.font_palettes = palette_registry.clone();
  style.font_palette = FontPalette::Named("--green".into());
  let style = Arc::new(style);

  let run = shape_single_run("A", &style, &font_ctx);
  assert!(!run.palette_overrides.is_empty());
  assert_ne!(run.palette_override_hash, 0);

  let width = 140;
  let height = 140;
  let start_x = 20.0;
  let baseline = 110.0;

  let marker = FragmentNode::new_with_style(
    Rect::from_xywh(start_x, 0.0, 100.0, height as f32),
    FragmentContent::Text {
      text: "A".to_string().into(),
      box_id: None,
      baseline_offset: baseline,
      shaped: Some(Arc::new(vec![run.clone()])),
      is_marker: true,
    },
    vec![],
    style,
  );
  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, width as f32, height as f32), vec![
    marker,
  ]);
  let list = DisplayListBuilder::new().build(&root);

  let dl_pixmap = DisplayListRenderer::new(width, height, Rgba::WHITE, font_ctx.clone())
    .unwrap()
    .render(&list)
    .expect("render list marker");

  let mut legacy = Pixmap::new(width, height).unwrap();
  legacy.fill(SkColor::WHITE);
  TextRasterizer::new()
    .render_shaped_run(&run, start_x, baseline, Rgba::BLACK, &mut legacy)
    .expect("render legacy marker");

  assert_eq!(
    dl_pixmap.data(),
    legacy.data(),
    "list marker display-list rendering should match legacy rasterizer output with palette overrides"
  );

  let hist = color_histogram(&dl_pixmap);
  assert!(hist.contains_key(&[0, 255, 0]), "expected green override pixels");
  assert!(
    !hist.contains_key(&[255, 0, 0]),
    "expected base red palette entry to be absent after overrides"
  );

  let marker_item = list
    .items()
    .iter()
    .find_map(|item| match item {
      DisplayItem::ListMarker(item) => Some(item),
      _ => None,
    })
    .expect("expected a list marker display item");
  assert_eq!(marker_item.palette_override_hash, run.palette_override_hash);
  assert_eq!(marker_item.palette_overrides.as_ref(), run.palette_overrides.as_ref());
}
