use std::path::PathBuf;
use std::sync::Arc;

use fastrender::layout::taffy_integration::{
  reset_taffy_counters, taffy_counters, total_taffy_invocations,
};
use fastrender::style::display::Display;
use fastrender::{
  BoxNode, ComputedStyle, FastRender, FormattingContextFactory, FormattingContextType,
  LayoutConstraints,
};

fn with_large_stack<F, R>(f: F) -> R
where
  F: FnOnce() -> R + Send + 'static,
  R: Send + 'static,
{
  const STACK_SIZE: usize = 16 * 1024 * 1024;
  let handle = std::thread::Builder::new()
    .stack_size(STACK_SIZE)
    .spawn(f)
    .expect("failed to spawn test thread");
  match handle.join() {
    Ok(result) => result,
    Err(payload) => std::panic::resume_unwind(payload),
  }
}

#[test]
fn flex_and_grid_route_through_taffy() {
  reset_taffy_counters();

  let mut flex_style = ComputedStyle::default();
  flex_style.display = Display::Flex;
  let flex_node = BoxNode::new_block(Arc::new(flex_style), FormattingContextType::Flex, vec![]);

  let mut grid_style = ComputedStyle::default();
  grid_style.display = Display::Grid;
  let grid_node = BoxNode::new_block(Arc::new(grid_style), FormattingContextType::Grid, vec![]);

  let factory = FormattingContextFactory::new();
  let constraints = LayoutConstraints::definite(800.0, 600.0);

  let flex_fc = factory.create(FormattingContextType::Flex);
  flex_fc.layout(&flex_node, &constraints).unwrap();

  let grid_fc = factory.create(FormattingContextType::Grid);
  grid_fc.layout(&grid_node, &constraints).unwrap();

  let counts = taffy_counters();
  assert!(counts.flex > 0, "flex containers must route through Taffy");
  assert!(counts.grid > 0, "grid containers must route through Taffy");
}

#[test]
fn tables_do_not_invoke_taffy() {
  with_large_stack(|| {
    reset_taffy_counters();

    let fixture =
      PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/html/table_auto.html");
    let html = std::fs::read_to_string(&fixture)
      .unwrap_or_else(|e| panic!("failed to load table fixture {}: {}", fixture.display(), e));

    let mut renderer = FastRender::new().expect("renderer");
    renderer
      .render_html(&html, 800, 800)
      .expect("table fixture should render");

    assert_eq!(
      total_taffy_invocations(),
      0,
      "table layout must not call into the Taffy adapter",
    );
  });
}
