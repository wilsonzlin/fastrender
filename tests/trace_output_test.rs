use fastrender::{FastRender, RenderOptions};

#[test]
fn trace_file_includes_pipeline_events() {
  const STACK_SIZE: usize = 64 * 1024 * 1024;

  std::thread::Builder::new()
    .name("trace_file_includes_pipeline_events".to_string())
    .stack_size(STACK_SIZE)
    .spawn(|| {
      let dir = tempfile::tempdir().expect("tempdir");
      let trace_path = dir.path().join("trace.json");

      let mut renderer = FastRender::new().expect("renderer");
      let options = RenderOptions::new()
        .with_viewport(64, 64)
        .with_trace_output(trace_path.clone());

      renderer
        .render_html_with_options("<div>trace me</div>", options)
        .expect("render");

      let data = std::fs::read_to_string(&trace_path).expect("trace output exists");
      assert!(data.contains("\"dom_parse\""), "dom parse span missing");
      assert!(data.contains("\"css_parse\""), "css parse span missing");
      assert!(data.contains("\"layout\""), "layout span missing");
      assert!(
        data.contains("\"display_list_build\""),
        "display list build span missing"
      );
      assert!(data.contains("\"rasterize\""), "rasterization span missing");
    })
    .expect("spawn render thread")
    .join()
    .expect("render thread panicked");
}
