use std::path::Path;

use fastrender::text::font_db::FontDatabase;
use fastrender::text::font_resolver::font_is_emoji_font;

fn load_font_fixture(path: &Path) -> (FontDatabase, fastrender::text::font_db::LoadedFont) {
  let mut db = FontDatabase::empty();
  let data = std::fs::read(path).unwrap_or_else(|err| panic!("read font {}: {err}", path.display()));
  db.load_font_data(data)
    .unwrap_or_else(|err| panic!("load font {}: {err:?}", path.display()));

  let id = db.faces().next().expect("font face").id;
  let font = db.load_font(id).expect("load font");
  (db, font)
}

#[test]
fn emoji_font_detection_uses_tables_for_web_fonts_even_with_neutral_family_name() {
  let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
  let (db, mut font) =
    load_font_fixture(&manifest_dir.join("tests/fixtures/fonts/FastRenderEmoji.ttf"));
  font.family = "CustomFont".to_string();

  assert!(
    font_is_emoji_font(&db, None, &font),
    "web emoji font should be detected via color tables even when family name is neutral"
  );
}

#[test]
fn emoji_font_detection_does_not_misclassify_text_web_fonts() {
  let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
  let (db, mut font) =
    load_font_fixture(&manifest_dir.join("tests/fixtures/fonts/DejaVuSans-subset.ttf"));
  font.family = "CustomFont".to_string();

  assert!(
    !font_is_emoji_font(&db, None, &font),
    "text fonts without color tables should not be detected as emoji when family name is neutral"
  );
}

