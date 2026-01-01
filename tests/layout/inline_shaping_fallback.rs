use std::sync::Arc;

use fastrender::css::types::{FontFaceRule, FontFaceSource};
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::layout::formatting_context::LayoutError;
use fastrender::resource::FetchedResource;
use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_loader::{FontContext, FontFetcher, WebFontPolicy};
use fastrender::{
  BoxNode, ComputedStyle, FormattingContext, FormattingContextType, FragmentContent,
};

#[derive(Clone)]
struct FixtureFontFetcher {
  font: Vec<u8>,
}

impl FontFetcher for FixtureFontFetcher {
  fn fetch(&self, url: &str) -> fastrender::Result<FetchedResource> {
    Ok(FetchedResource::with_final_url(
      self.font.clone(),
      Some("font/ttf".to_string()),
      Some(url.to_string()),
    ))
  }
}

fn find_first_text_fragment(node: &fastrender::FragmentNode) -> Option<&fastrender::FragmentNode> {
  if matches!(node.content, FragmentContent::Text { .. }) {
    return Some(node);
  }
  for child in &node.children {
    if let Some(found) = find_first_text_fragment(child) {
      return Some(found);
    }
  }
  None
}

#[test]
fn inline_layout_degrades_gracefully_when_shaping_fails() {
  let font_bytes = include_bytes!("../fixtures/fonts/NotoSans-subset.ttf").to_vec();

  let db = FontDatabase::empty();
  let fetcher = Arc::new(FixtureFontFetcher { font: font_bytes });
  let font_context = FontContext::with_database_and_fetcher(Arc::new(db), fetcher);

  // Deliberately register the font as a web font while leaving the system/bundled database empty.
  // When shaping hits characters that aren't covered by this web font, the shaping pipeline can
  // fail to find a last-resort database font. Inline layout should recover by synthesizing
  // placeholder runs rather than aborting with MissingContext.
  let face = FontFaceRule {
    family: Some("sans-serif".to_string()),
    sources: vec![FontFaceSource::url(
      "http://example.com/noto.ttf".to_string(),
    )],
    ..Default::default()
  };
  font_context
    .load_web_fonts_with_policy(&[face], None, None, WebFontPolicy::Swap)
    .expect("web font should load");

  let mut text_style = ComputedStyle::default();
  text_style.font_family = vec!["sans-serif".to_string()].into();
  text_style.font_size = 16.0;
  let text_node = BoxNode::new_text(Arc::new(text_style), "뮝\u{07FD}".to_string());

  let root = BoxNode::new_block(
    Arc::new(ComputedStyle::default()),
    FormattingContextType::Inline,
    vec![text_node],
  );

  let ifc = InlineFormattingContext::with_font_context(font_context);
  let fragment = match ifc.layout(
    &root,
    &fastrender::LayoutConstraints::definite(200.0, 100.0),
  ) {
    Ok(fragment) => fragment,
    Err(LayoutError::MissingContext(msg)) => {
      panic!("unexpected MissingContext from shaping: {msg}")
    }
    Err(err) => panic!("layout failed unexpectedly: {err:?}"),
  };

  let text_fragment = find_first_text_fragment(&fragment).expect("expected a text fragment");
  assert!(
    text_fragment.bounds.width() > 0.0,
    "expected non-zero inline advance for text"
  );
}
