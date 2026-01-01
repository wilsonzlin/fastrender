use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::{
  CssRule, Declaration, FontPaletteBase, FontPaletteOverride, PropertyValue,
};
use fastrender::paint::text_rasterize::TextRasterizer;
use fastrender::style::color::{Color, Rgba};
use fastrender::style::font_palette::FontPaletteRegistry;
use fastrender::style::media::MediaContext;
use fastrender::style::properties::{apply_declaration_with_base, DEFAULT_VIEWPORT};
use fastrender::style::types::FontPalette;
use fastrender::style::ComputedStyle;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::FontContext;
use fastrender::text::pipeline::ShapingPipeline;
use tiny_skia::Pixmap;

#[test]
fn parses_font_palette_values_rules() {
  let css = "@font-palette-values --emoji { font-family: Foo, \"Bar\"; base-palette: dark; override-colors: 0 rgb(10 20 30), 3 currentColor; }";
  let sheet = parse_stylesheet(css).expect("stylesheet should parse");
  assert_eq!(sheet.rules.len(), 1);

  let rule = match &sheet.rules[0] {
    CssRule::FontPaletteValues(rule) => rule,
    other => panic!("unexpected rule parsed: {other:?}"),
  };

  assert_eq!(rule.name, "--emoji");
  assert_eq!(
    rule.font_families,
    vec!["Foo".to_string(), "Bar".to_string()]
  );
  assert_eq!(rule.base_palette, FontPaletteBase::Dark);
  assert_eq!(
    rule.overrides,
    vec![
      FontPaletteOverride {
        index: 0,
        color: Color::Rgba(Rgba::from_rgba8(10, 20, 30, 255))
      },
      FontPaletteOverride {
        index: 3,
        color: Color::CurrentColor
      }
    ]
  );
}

#[test]
fn font_palette_property_accepts_named_identifiers() {
  let mut styles = ComputedStyle::default();
  let parent = ComputedStyle::default();
  let declaration = Declaration {
    property: "font-palette".into(),
    value: PropertyValue::Keyword("--Custom-Palette".into()),
    raw_value: "--Custom-Palette".into(),
    important: false,
  };

  apply_declaration_with_base(
    &mut styles,
    &declaration,
    &parent,
    &ComputedStyle::default(),
    None,
    parent.font_size,
    parent.root_font_size,
    DEFAULT_VIEWPORT,
  );

  assert_eq!(
    styles.font_palette,
    FontPalette::Named("--custom-palette".into())
  );
}

fn load_test_font() -> (FontContext, String) {
  let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fonts/ColorTestCOLR.ttf");
  let bytes = std::fs::read(path).expect("test color font should load");

  let mut db = FontDatabase::empty();
  db.load_font_data(bytes).expect("font should parse");
  let family = db
    .first_font()
    .expect("font should be present")
    .family
    .clone();

  (FontContext::with_database(Arc::new(db)), family)
}

fn build_palette_registry(family: &str) -> Arc<FontPaletteRegistry> {
  let css = format!(
    "@font-palette-values --custom {{ font-family: {family}; base-palette: 0; override-colors: 1 rgb(0 255 0); }}"
  );
  let sheet = parse_stylesheet(&css).expect("palette rule should parse");
  let mut collected = sheet.collect_font_palette_rules(&MediaContext::default());
  assert_eq!(collected.len(), 1, "expected a single palette rule");

  let mut registry = FontPaletteRegistry::default();
  registry.register(collected.pop().unwrap().rule.clone());
  Arc::new(registry)
}

fn build_current_color_palette_registry(family: &str) -> Arc<FontPaletteRegistry> {
  let css = format!(
    "@font-palette-values --current {{ font-family: {family}; base-palette: 0; override-colors: 1 currentColor; }}"
  );
  let sheet = parse_stylesheet(&css).expect("palette rule should parse");
  let mut collected = sheet.collect_font_palette_rules(&MediaContext::default());
  assert_eq!(collected.len(), 1, "expected a single palette rule");

  let mut registry = FontPaletteRegistry::default();
  registry.register(collected.pop().unwrap().rule.clone());
  Arc::new(registry)
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

#[test]
fn palette_overrides_recolor_colr_glyphs() {
  let (font_context, family) = load_test_font();
  let palette_registry = build_palette_registry(&family);

  let mut normal_style = ComputedStyle::default();
  normal_style.font_family = vec![family.clone()].into();
  normal_style.font_size = 32.0;
  normal_style.font_palettes = palette_registry.clone();
  normal_style.font_palette = FontPalette::Normal;

  let mut custom_style = normal_style.clone();
  custom_style.font_palette = FontPalette::Named("--custom".into());

  let pipeline = ShapingPipeline::new();
  let normal_runs = pipeline
    .shape("A", &normal_style, &font_context)
    .expect("shaping should succeed");
  let custom_runs = pipeline
    .shape("A", &custom_style, &font_context)
    .expect("shaping should succeed");

  assert!(!normal_runs.is_empty());
  assert!(normal_runs[0].palette_overrides.is_empty());
  assert_eq!(
    custom_runs[0].palette_overrides.as_ref(),
    &vec![(1, Rgba::from_rgba8(0, 255, 0, 255))]
  );

  let mut rasterizer = TextRasterizer::new();
  let mut normal_pixmap = Pixmap::new(64, 64).unwrap();
  rasterizer
    .render_runs(
      &normal_runs,
      8.0,
      48.0,
      normal_style.color,
      &mut normal_pixmap,
    )
    .unwrap();

  let mut custom_pixmap = Pixmap::new(64, 64).unwrap();
  rasterizer
    .render_runs(
      &custom_runs,
      8.0,
      48.0,
      custom_style.color,
      &mut custom_pixmap,
    )
    .unwrap();

  let red = [255, 0, 0];
  let blue = [0, 0, 255];
  let green = [0, 255, 0];

  let normal_hist = color_histogram(&normal_pixmap);
  let custom_hist = color_histogram(&custom_pixmap);

  assert!(normal_hist.contains_key(&red));
  assert!(normal_hist.contains_key(&blue));
  assert!(!normal_hist.contains_key(&green));

  assert!(custom_hist.contains_key(&green));
  assert!(custom_hist.contains_key(&blue));
  assert!(!custom_hist.contains_key(&red));
}

#[test]
fn palette_overrides_follow_current_color_across_cached_runs() {
  let (font_context, family) = load_test_font();
  let palette_registry = build_current_color_palette_registry(&family);

  let mut red_style = ComputedStyle::default();
  red_style.font_family = vec![family.clone()].into();
  red_style.font_size = 32.0;
  red_style.font_palette = FontPalette::Named("--current".into());
  red_style.font_palettes = palette_registry.clone();
  red_style.color = Rgba::from_rgba8(200, 0, 0, 255);

  let pipeline = ShapingPipeline::new();
  let red_runs = pipeline
    .shape("A", &red_style, &font_context)
    .expect("shaping should succeed");
  assert_eq!(
    red_runs[0].palette_overrides.as_ref(),
    &vec![(1, red_style.color)]
  );

  let mut green_style = red_style.clone();
  green_style.color = Rgba::from_rgba8(0, 200, 0, 255);

  let green_runs = pipeline
    .shape("A", &green_style, &font_context)
    .expect("shaping should succeed");
  assert_eq!(
    green_runs[0].palette_overrides.as_ref(),
    &vec![(1, green_style.color)]
  );
}
