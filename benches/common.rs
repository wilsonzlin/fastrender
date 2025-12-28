use std::sync::{Arc, OnceLock};

use fastrender::{
  css::parser::{
    extract_css_sources, parse_stylesheet, rel_list_contains_stylesheet, StylesheetSource,
  },
  css::types::StyleSheet,
  dom::{parse_html, DomNode},
  geometry::Rect,
  paint::{
    display_list::DisplayList, display_list_builder::DisplayListBuilder,
    display_list_renderer::DisplayListRenderer, optimize::DisplayListOptimizer,
  },
  style::{
    cascade::{apply_styles_with_media, StyledNode},
    color::Rgba,
    media::{MediaContext, MediaQuery, MediaQueryCache},
  },
  text::{font_db::FontDatabase, font_loader::FontContext},
  tree::{box_generation::generate_box_tree, box_tree::BoxTree, fragment_tree::FragmentTree},
  LayoutConfig, LayoutEngine, Pixmap, Size,
};

pub const BLOCK_SIMPLE_HTML: &str = include_str!("../tests/fixtures/html/block_simple.html");
pub const FLEX_HTML: &str = include_str!("../tests/fixtures/html/flex_grow_shrink.html");
pub const GRID_HTML: &str = include_str!("../tests/fixtures/html/grid_template.html");
pub const TABLE_HTML: &str = include_str!("../tests/fixtures/html/table_span.html");
pub const FORM_CONTROLS_HTML: &str = include_str!("../tests/fixtures/html/form_controls.html");

pub const SMALL_VIEWPORT: (u32, u32) = (800, 600);
pub const REALISTIC_VIEWPORT: (u32, u32) = (1100, 900);

static FIXED_FONT_CONTEXT: OnceLock<FontContext> = OnceLock::new();
const FIXED_FONT_TTF: &[u8] = include_bytes!("../tests/fixtures/fonts/DejaVuSans-subset.ttf");
const FIXED_FONT_WOFF2: &[u8] = include_bytes!("../tests/fixtures/fonts/DejaVuSans-subset.woff2");

pub fn fixed_font_context() -> FontContext {
  FIXED_FONT_CONTEXT
    .get_or_init(|| {
      let mut db = FontDatabase::empty();
      db.load_font_data(FIXED_FONT_TTF.to_vec())
        .expect("load bundled TTF");
      let _ = db.load_font_data(FIXED_FONT_WOFF2.to_vec());
      FontContext::with_database(Arc::new(db))
    })
    .clone()
}

pub fn parse_dom(html: &str) -> DomNode {
  parse_html(html).expect("parse DOM")
}

pub fn media_context(viewport: (u32, u32)) -> MediaContext {
  MediaContext::screen(viewport.0 as f32, viewport.1 as f32)
}

fn stylesheet_type_is_css(type_attr: Option<&str>) -> bool {
  match type_attr {
    None => true,
    Some(value) => {
      let mime = value.split(';').next().map(str::trim).unwrap_or("");
      mime.is_empty() || mime.eq_ignore_ascii_case("text/css")
    }
  }
}

fn media_matches(
  media: Option<&str>,
  media_ctx: &MediaContext,
  cache: &mut MediaQueryCache,
) -> bool {
  let Some(raw) = media else {
    return true;
  };
  if raw.trim().is_empty() {
    return true;
  }
  if let Ok(queries) = MediaQuery::parse_list(raw) {
    return media_ctx.evaluate_list_with_cache(&queries, Some(cache));
  }
  false
}

pub fn inline_css_text(dom: &DomNode, media_ctx: &MediaContext) -> String {
  let mut css_text = String::new();
  let mut cache = MediaQueryCache::default();

  for scoped in extract_css_sources(dom) {
    match scoped.source {
      StylesheetSource::Inline(inline) => {
        if inline.disabled || inline.css.trim().is_empty() {
          continue;
        }
        if !stylesheet_type_is_css(inline.type_attr.as_deref()) {
          continue;
        }
        if !media_matches(inline.media.as_deref(), media_ctx, &mut cache) {
          continue;
        }
        css_text.push_str(&inline.css);
        css_text.push('\n');
      }
      StylesheetSource::External(link) => {
        // Benchmarks stay offline; skip external stylesheets.
        if link.disabled
          || link.href.trim().is_empty()
          || !rel_list_contains_stylesheet(&link.rel)
          || !stylesheet_type_is_css(link.type_attr.as_deref())
        {
          continue;
        }
      }
    }
  }

  css_text
}

pub fn stylesheet_for_dom(dom: &DomNode, media_ctx: &MediaContext) -> StyleSheet {
  let mut rules = Vec::new();
  let mut cache = MediaQueryCache::default();

  for scoped in extract_css_sources(dom) {
    match scoped.source {
      StylesheetSource::Inline(inline) => {
        if inline.disabled || inline.css.trim().is_empty() {
          continue;
        }
        if !stylesheet_type_is_css(inline.type_attr.as_deref()) {
          continue;
        }
        if !media_matches(inline.media.as_deref(), media_ctx, &mut cache) {
          continue;
        }
        if let Ok(sheet) = parse_stylesheet(&inline.css) {
          rules.extend(sheet.rules);
        }
      }
      StylesheetSource::External(link) => {
        if link.disabled
          || link.href.trim().is_empty()
          || !rel_list_contains_stylesheet(&link.rel)
          || !stylesheet_type_is_css(link.type_attr.as_deref())
        {
          continue;
        }
      }
    }
  }

  StyleSheet { rules }
}

pub fn cascade(dom: &DomNode, stylesheet: &StyleSheet, media_ctx: &MediaContext) -> StyledNode {
  apply_styles_with_media(dom, stylesheet, media_ctx)
}

pub fn box_tree_from_styled(styled: &StyledNode) -> BoxTree {
  generate_box_tree(styled)
}

pub fn layout_engine(viewport: (u32, u32), font_ctx: &FontContext) -> LayoutEngine {
  let config = LayoutConfig::for_viewport(Size::new(viewport.0 as f32, viewport.1 as f32));
  LayoutEngine::with_font_context(config, font_ctx.clone())
}

pub fn layout_fragment_tree(engine: &LayoutEngine, box_tree: &BoxTree) -> FragmentTree {
  engine.layout_tree(box_tree).expect("layout tree")
}

pub fn build_display_list(fragments: &FragmentTree, font_ctx: &FontContext) -> DisplayList {
  DisplayListBuilder::new()
    .with_font_context(font_ctx.clone())
    .with_device_pixel_ratio(1.0)
    .build_tree(fragments)
}

pub fn optimize_display_list(list: &DisplayList, viewport: (u32, u32)) -> DisplayList {
  let viewport_rect = Rect::from_xywh(0.0, 0.0, viewport.0 as f32, viewport.1 as f32);
  DisplayListOptimizer::new()
    .optimize(list.clone(), viewport_rect)
    .0
}

pub fn rasterize_display_list(
  list: &DisplayList,
  viewport: (u32, u32),
  font_ctx: &FontContext,
) -> Pixmap {
  DisplayListRenderer::new(viewport.0, viewport.1, Rgba::WHITE, font_ctx.clone())
    .expect("renderer")
    .render(list)
    .expect("render display list")
}

pub fn render_pipeline(html: &str, viewport: (u32, u32), font_ctx: &FontContext) -> Pixmap {
  let media_ctx = media_context(viewport);
  let dom = parse_dom(html);
  let stylesheet = stylesheet_for_dom(&dom, &media_ctx);
  let styled = cascade(&dom, &stylesheet, &media_ctx);
  let box_tree = box_tree_from_styled(&styled);
  let engine = layout_engine(viewport, font_ctx);
  let fragments = layout_fragment_tree(&engine, &box_tree);
  let display_list = build_display_list(&fragments, font_ctx);
  let optimized = optimize_display_list(&display_list, viewport);
  rasterize_display_list(&optimized, viewport, font_ctx)
}

pub fn parse_stylesheet_text(css: &str) -> StyleSheet {
  parse_stylesheet(css).expect("parse stylesheet")
}
