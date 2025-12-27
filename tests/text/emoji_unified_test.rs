use fastrender::text::emoji;
use fastrender::text::font_db::FontDatabase;

#[test]
fn font_database_and_module_emoji_detection_match() {
  let cases = [
    ('😀', "emoji presentation face"),
    ('🚀', "emoji presentation rocket"),
    ('#', "keycap base (text-default emoji)"),
    ('©', "text-default emoji"),
    ('🇺', "regional indicator"),
    ('\u{200D}', "zero width joiner"),
    ('\u{FE0F}', "emoji variation selector"),
    ('\u{FE0E}', "text variation selector"),
    ('A', "latin letter"),
    ('中', "cjk character"),
  ];

  for (ch, label) in cases {
    assert_eq!(
      emoji::is_emoji(ch),
      FontDatabase::is_emoji(ch),
      "Emoji detection mismatch for {label} ({:?})",
      ch
    );
  }
}
