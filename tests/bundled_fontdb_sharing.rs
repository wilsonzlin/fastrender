use std::sync::Arc;

use fastrender::text::font_db::FontConfig;
use fastrender::text::font_loader::FontContext;

#[test]
fn bundled_fontdb_is_shared_between_contexts() {
  #[cfg(debug_assertions)]
  let _guard = fastrender::text::font_db::FaceParseCountGuard::start();

  let ctx1 = FontContext::with_config(FontConfig::bundled_only());
  let ctx2 = FontContext::with_config(FontConfig::bundled_only());

  assert!(ctx1.font_count() > 0);
  assert_eq!(ctx1.font_count(), ctx2.font_count());

  let db1 = ctx1.database().shared_db();
  let db2 = ctx2.database().shared_db();
  assert!(Arc::ptr_eq(&db1, &db2));

  // Creating bundled-only contexts should not require parsing faces (coverage queries, etc.).
  // The metadata parsing happens once inside `fontdb` when the shared database is initialised.
  #[cfg(debug_assertions)]
  assert_eq!(fastrender::text::font_db::face_parse_count(), 0);
}
