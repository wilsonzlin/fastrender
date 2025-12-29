use std::time::Duration;

use fastrender::css::parser::parse_stylesheet;
use fastrender::css::types::CssImportLoader;
use fastrender::error::{RenderError, RenderStage};
use fastrender::render_control::{with_deadline, RenderDeadline};
use fastrender::style::media::MediaContext;

struct SleepyImportLoader {
  sleep: Duration,
}

impl CssImportLoader for SleepyImportLoader {
  fn load(&self, url: &str) -> fastrender::error::Result<String> {
    std::thread::sleep(self.sleep);
    let depth = url
      .rsplit('/')
      .next()
      .and_then(|part| part.strip_suffix(".css"))
      .and_then(|num| num.parse::<usize>().ok())
      .unwrap_or(0);
    if depth == 0 {
      Ok("body { color: black; }".to_string())
    } else {
      Ok(format!(
        "@import \"https://example.com/{}.css\";",
        depth.saturating_sub(1)
      ))
    }
  }
}

#[test]
fn css_import_resolution_times_out_with_deadline() {
  let base_sheet = parse_stylesheet("@import \"https://example.com/25.css\";").unwrap();
  let loader = SleepyImportLoader {
    sleep: Duration::from_millis(2),
  };
  let media_ctx = MediaContext::screen(800.0, 600.0);
  let deadline = RenderDeadline::new(Some(Duration::from_millis(1)), None);

  let result = with_deadline(Some(&deadline), || {
    base_sheet.resolve_imports_with_cache(
      &loader,
      Some("https://example.com/index.html"),
      &media_ctx,
      None,
    )
  });

  match result {
    Err(RenderError::Timeout {
      stage: RenderStage::Css,
      ..
    }) => {}
    other => panic!("expected CSS timeout, got {:?}", other),
  }
}
