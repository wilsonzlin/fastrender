use fastrender::css::types::StyleSheet;
use fastrender::debug::snapshot::snapshot_pipeline;
use fastrender::dom;
use fastrender::geometry::Size;
use fastrender::layout::engine::{LayoutConfig, LayoutEngine};
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::style::cascade::apply_styles;
use fastrender::text::font_db::FontConfig;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::box_generation::generate_box_tree_with_anonymous_fixup;

#[test]
fn pipeline_snapshot_matches_fixture() {
  const STACK_SIZE: usize = 64 * 1024 * 1024;
  let handle = std::thread::Builder::new()
    .name("debug-snapshot-test".to_string())
    .stack_size(STACK_SIZE)
    .spawn(|| {
      let html = r#"
    <!doctype html>
    <html>
      <body style="margin: 0">
        <div id="root" style="position: relative; z-index: 2; overflow: hidden; width: 120px; height: 60px; padding: 4px; margin: 8px; border: 2px solid rgb(10, 20, 30); background: rgb(200, 210, 220);">
          <span class="child" style="display: inline-block; position: absolute; left: 6px; top: 10px; padding: 2px; border: 1px dashed rgb(50, 60, 70); color: rgb(5, 6, 7);">Hi</span>
          <p style="margin: 2px 0 0 0;">Bye</p>
        </div>
      </body>
    </html>
  "#;

      let dom = dom::parse_html(html).expect("parse html");
      let stylesheet = StyleSheet::new();
      let styled = apply_styles(&dom, &stylesheet);
      let box_tree = generate_box_tree_with_anonymous_fixup(&styled).unwrap();

      let font_context = FontContext::with_config(
        FontConfig::new()
          .with_system_fonts(false)
          .with_bundled_fonts(true),
      );
      let engine = LayoutEngine::with_font_context(
        LayoutConfig::for_viewport(Size::new(200.0, 200.0)),
        font_context,
      );
      let fragment_tree = engine.layout_tree(&box_tree).expect("layout");

      let mut display_list =
        DisplayListBuilder::new().build_with_stacking_tree(&fragment_tree.root);
      for extra in &fragment_tree.additional_fragments {
        let extra_list = DisplayListBuilder::new().build_with_stacking_tree(extra);
        display_list.append(extra_list);
      }

      let snapshot = snapshot_pipeline(&dom, &styled, &box_tree, &fragment_tree, &display_list);
      let actual = serde_json::to_string_pretty(&snapshot).unwrap();
      let expected = include_str!("fixtures/snapshots/basic.json");

      if std::env::var_os("UPDATE_SNAPSHOTS").is_some() {
        let path = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
          .join("tests/fixtures/snapshots/basic.json");
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, &actual).unwrap();
      }

      assert_eq!(actual, expected);
    })
    .expect("spawn snapshot test");

  match handle.join() {
    Ok(()) => {}
    Err(panic) => std::panic::resume_unwind(panic),
  }
}
