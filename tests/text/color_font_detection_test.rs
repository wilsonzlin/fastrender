use std::path::Path;

use fastrender::text::font_db::FontDatabase;
use fontdb::ID;

fn load_test_fonts() -> (FontDatabase, ID, ID) {
  let mut db = FontDatabase::empty();
  let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));

  let color_font_path = manifest_dir.join("tests/fonts/ColorTestCOLR.ttf");
  let color_data = std::fs::read(&color_font_path).expect("read color font");
  db.load_font_data(color_data).expect("load color test font");

  let text_font_path = manifest_dir.join("tests/fixtures/fonts/DejaVuSans-subset.ttf");
  let text_data = std::fs::read(&text_font_path).expect("read dejavu font");
  db.load_font_data(text_data).expect("load dejavu font");

  let mut color_font_id = None;
  let mut text_font_id = None;

  for face in db.faces() {
    if face
      .families
      .iter()
      .any(|(name, _)| name.contains("ColorTestCOLR"))
    {
      color_font_id = Some(face.id);
    }
    if face
      .families
      .iter()
      .any(|(name, _)| name.contains("DejaVu Sans"))
    {
      text_font_id = Some(face.id);
    }
  }

  (
    db,
    color_font_id.expect("color font id"),
    text_font_id.expect("dejavu font id"),
  )
}

#[test]
fn detects_color_capable_fonts_via_tables() {
  let (db, color_font_id, text_font_id) = load_test_fonts();

  assert_eq!(db.is_color_capable_font(color_font_id), Some(true));
  assert_eq!(db.is_color_capable_font(text_font_id), Some(false));
}

#[test]
fn find_emoji_fonts_prefers_color_tables() {
  let (db, color_font_id, text_font_id) = load_test_fonts();
  let emoji_fonts = db.find_emoji_fonts();

  assert!(
    emoji_fonts.contains(&color_font_id),
    "color font should be treated as emoji-capable"
  );
  assert!(
    !emoji_fonts.contains(&text_font_id),
    "text font without color tables should not be classified as emoji"
  );
}
