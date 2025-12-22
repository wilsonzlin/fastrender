use fastrender::FastRender;
use std::path::PathBuf;
use tempfile::tempdir;
use url::Url;

#[test]
fn iframe_file_src_renders_nested_document() {
  let temp = tempdir().unwrap();
  let inner_path = temp.path().join("inner.html");
  std::fs::write(
    &inner_path,
    "<!doctype html><style>html, body { margin: 0; background: rgb(220, 0, 0); }</style>",
  )
  .unwrap();

  let src = Url::from_file_path(&inner_path).unwrap().to_string();

  let parent_html = format!(
    r#"<!doctype html><html><body style=\"margin:0; background: rgb(0, 128, 0);\">\
<iframe src=\"{src}\" style=\"width:48px; height:48px; border:0;\"></iframe>\
</body></html>"#
  );

  let mut renderer = FastRender::new().unwrap();
  let rendered = renderer.render_to_png(&parent_html, 64, 64).unwrap();

  let golden_path =
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/golden/iframe_file.png");

  if std::env::var("UPDATE_GOLDEN").is_ok() {
    std::fs::write(&golden_path, &rendered).unwrap();
  }

  let expected = std::fs::read(&golden_path).expect("golden image");
  assert_eq!(rendered, expected, "iframe rendering should match golden");
}
