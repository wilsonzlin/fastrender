//! Public API tests for FastRender
//!
//! Tests for the FastRender public API (W6.T03).
//! These tests verify that the public API works correctly and covers
//! all major use cases.
//!
//! Note: Pipeline tests explicitly use bundled fonts and disallow HTTP(S)
//! fetches so they remain deterministic in CI.

mod test_public_api {
use fastrender::api::{FastRender, FastRenderConfig, FastRenderPool, FastRenderPoolConfig};
use fastrender::compat::CompatProfile;
use fastrender::debug::runtime::RuntimeToggles;
use fastrender::dom::DomCompatibilityMode;
use fastrender::{FontConfig, LayoutParallelism, PaintParallelism, ResourcePolicy, Rgba};

fn deterministic_config() -> FastRenderConfig {
  FastRenderConfig::new()
    // Ensure deterministic behavior regardless of any FASTR_* env vars in the test runner.
    .with_runtime_toggles(RuntimeToggles::default())
    // Keep defaults small so accidental default rendering stays fast.
    .with_default_viewport(128, 128)
    // Avoid scanning system fonts (and keep font metrics stable).
    .with_font_sources(FontConfig::bundled_only())
    // Tests must not reach the network.
    .with_resource_policy(ResourcePolicy::default().allow_http(false).allow_https(false))
    // Avoid spawning rayon work for tiny test documents and keep execution predictable.
    .with_paint_parallelism(PaintParallelism::disabled())
    .with_layout_parallelism(LayoutParallelism::disabled())
}

fn deterministic_renderer() -> FastRender {
  FastRender::with_config(deterministic_config()).expect("create deterministic renderer")
}

fn assert_png_header(png_bytes: &[u8]) {
  const PNG_HEADER: &[u8] = b"\x89PNG\r\n\x1a\n";
  assert!(
    png_bytes.starts_with(PNG_HEADER),
    "PNG encoding should produce a valid PNG header"
  );
}

// =============================================================================
// FastRender Creation Tests
// =============================================================================

#[test]
fn test_fastrender_new() {
  // Avoid calling `FastRender::new()` here because it uses `FontConfig::default()`,
  // which can trigger system font discovery depending on environment variables.
  //
  // We still want coverage that the public API exists and has the expected type.
  let _new_fn: fn() -> fastrender::Result<FastRender> = FastRender::new;
}

#[test]
fn test_fastrender_with_default_config() {
  let config = FastRenderConfig::default()
    .with_runtime_toggles(RuntimeToggles::default())
    .with_font_sources(FontConfig::bundled_only())
    .with_resource_policy(ResourcePolicy::default().allow_http(false).allow_https(false))
    .with_paint_parallelism(PaintParallelism::disabled())
    .with_layout_parallelism(LayoutParallelism::disabled());
  let result = FastRender::with_config(config);
  assert!(
    result.is_ok(),
    "FastRender::with_config(default) should succeed"
  );
}

#[test]
fn test_fastrender_with_custom_config() {
  let config = FastRenderConfig::new()
    .with_default_background(Rgba::rgb(240, 240, 240))
    .with_default_viewport(1920, 1080)
    .with_runtime_toggles(RuntimeToggles::default())
    .with_font_sources(FontConfig::bundled_only())
    .with_resource_policy(ResourcePolicy::default().allow_http(false).allow_https(false))
    .with_paint_parallelism(PaintParallelism::disabled())
    .with_layout_parallelism(LayoutParallelism::disabled());

  let result = FastRender::with_config(config);
  assert!(
    result.is_ok(),
    "FastRender::with_config(custom) should succeed"
  );

  let renderer = result.unwrap();
  assert_eq!(renderer.background_color().r, 240);
  assert_eq!(renderer.background_color().g, 240);
  assert_eq!(renderer.background_color().b, 240);
}

#[test]
fn test_thread_safe_pool_creation() {
  let pool = FastRenderPool::with_config(
    FastRenderPoolConfig::new()
      .with_renderer_config(deterministic_config())
      .with_pool_size(1),
  )
  .expect("pool");
  let pixmap = pool
    .render_html("<div>pool</div>", 64, 64)
    .expect("render html");
  assert_eq!(pixmap.width(), 64);
  assert_eq!(pixmap.height(), 64);
}

#[test]
fn test_builder_chain_for_compatibility() {
  let renderer = FastRender::builder()
    .compat_mode(CompatProfile::Standards)
    .with_site_compat_hacks()
    .font_sources(FontConfig::bundled_only())
    .resource_policy(ResourcePolicy::default().allow_http(false).allow_https(false))
    .paint_parallelism(PaintParallelism::disabled())
    .runtime_toggles(RuntimeToggles::default())
    .build();

  assert!(renderer.is_ok(), "Builder should produce a renderer");
}

// =============================================================================
// Configuration Tests
// =============================================================================

#[test]
fn test_config_default_values() {
  let config = FastRenderConfig::default();
  assert_eq!(config.background_color, Rgba::WHITE);
  assert_eq!(config.default_width, 800);
  assert_eq!(config.default_height, 600);
  assert_eq!(config.dom_compat_mode, DomCompatibilityMode::Standard);
}

#[test]
fn test_config_builder_pattern() {
  let config = FastRenderConfig::new()
    .with_default_background(Rgba::rgb(100, 150, 200))
    .with_default_viewport(1024, 768)
    .with_dom_compat_mode(DomCompatibilityMode::Compatibility);

  assert_eq!(config.background_color.r, 100);
  assert_eq!(config.background_color.g, 150);
  assert_eq!(config.background_color.b, 200);
  assert_eq!(config.default_width, 1024);
  assert_eq!(config.default_height, 768);
  assert_eq!(config.dom_compat_mode, DomCompatibilityMode::Compatibility);
}

#[test]
fn test_config_compatibility_chain() {
  let config = FastRenderConfig::new()
    .with_dom_compat_mode(DomCompatibilityMode::Compatibility)
    .with_meta_viewport(true)
    .compat_profile(CompatProfile::Standards);

  assert_eq!(config.dom_compat_mode, DomCompatibilityMode::Compatibility);
  assert!(config.apply_meta_viewport);
  assert_eq!(config.compat_profile, CompatProfile::Standards);

  let with_hacks = config.with_site_compat_hacks();
  assert_eq!(with_hacks.compat_profile, CompatProfile::SiteCompatibility);
}

// =============================================================================
// HTML Parsing Tests
// =============================================================================

#[test]
fn test_parse_html_simple() {
  let renderer = deterministic_renderer();
  let result = renderer.parse_html("<div>Hello</div>");
  assert!(result.is_ok(), "Simple HTML should parse successfully");
}

#[test]
fn test_parse_html_full_document() {
  let renderer = deterministic_renderer();
  let html = r#"
        <!DOCTYPE html>
        <html>
            <head>
                <title>Test</title>
            </head>
            <body>
                <h1>Hello World</h1>
                <p>This is a paragraph.</p>
            </body>
        </html>
    "#;

  let result = renderer.parse_html(html);
  assert!(
    result.is_ok(),
    "Full HTML document should parse successfully"
  );
}

#[test]
fn test_parse_html_with_style() {
  let renderer = deterministic_renderer();
  let html = r#"
        <html>
            <head>
                <style>
                    body { background: white; }
                    .box { width: 100px; height: 100px; background: red; }
                </style>
            </head>
            <body>
                <div class="box"></div>
            </body>
        </html>
    "#;

  let result = renderer.parse_html(html);
  assert!(
    result.is_ok(),
    "HTML with embedded CSS should parse successfully"
  );
}

#[test]
fn test_parse_html_empty() {
  let renderer = deterministic_renderer();
  let result = renderer.parse_html("");
  assert!(result.is_ok(), "Empty HTML should parse successfully");
}

// =============================================================================
// Component Access Tests
// =============================================================================

#[test]
fn test_font_context_access() {
  let renderer = deterministic_renderer();
  let _font_context = renderer.font_context();
  // Just verify we can access it
}

#[test]
fn test_font_context_mut_access() {
  let mut renderer = deterministic_renderer();
  let _font_context = renderer.font_context_mut();
  // Just verify we can access it
}

#[test]
fn test_layout_engine_access() {
  let renderer = deterministic_renderer();
  let _layout_engine = renderer.layout_engine();
  // Just verify we can access it
}

#[test]
fn test_background_color_get_set() {
  let mut renderer = deterministic_renderer();

  // Default is white
  assert_eq!(renderer.background_color(), Rgba::WHITE);

  // Set to custom color
  renderer.set_background_color(Rgba::rgb(50, 100, 150));
  assert_eq!(renderer.background_color().r, 50);
  assert_eq!(renderer.background_color().g, 100);
  assert_eq!(renderer.background_color().b, 150);
}

// =============================================================================
// Validation Tests
// =============================================================================

#[test]
fn test_render_html_invalid_dimensions() {
  let mut renderer = deterministic_renderer();

  // Zero width
  let result = renderer.render_html("<div>Test</div>", 0, 600);
  assert!(result.is_err(), "Zero width should return error");

  // Zero height
  let result = renderer.render_html("<div>Test</div>", 800, 0);
  assert!(result.is_err(), "Zero height should return error");

  // Both zero
  let result = renderer.render_html("<div>Test</div>", 0, 0);
  assert!(result.is_err(), "Both zero should return error");
}

// =============================================================================
// Re-export Tests
// =============================================================================

#[test]
fn test_reexports_from_lib() {
  // Verify that important types are re-exported from the crate root
  use fastrender::FastRender;
  use fastrender::FastRenderConfig;
  use fastrender::Pixmap;

  let config = FastRenderConfig::new()
    .with_runtime_toggles(RuntimeToggles::default())
    .with_font_sources(FontConfig::bundled_only())
    .with_resource_policy(ResourcePolicy::default().allow_http(false).allow_https(false))
    .with_paint_parallelism(PaintParallelism::disabled())
    .with_layout_parallelism(LayoutParallelism::disabled());
  let renderer = FastRender::with_config(config);
  assert!(renderer.is_ok());

  // Pixmap type is accessible
  let _: Option<Pixmap> = None;
}

#[test]
fn test_fragment_tree_type_reexport() {
  // FragmentTree type should be accessible
  use fastrender::FragmentTree;

  // Type is accessible (we can't create one without layout)
  let _: Option<FragmentTree> = None;
}

#[test]
fn test_rgba_available() {
  // style::Rgba should be accessible for public API
  use fastrender::Rgba;
  let color = Rgba::rgb(100, 150, 200);
  assert_eq!(color.r, 100);
}

// =============================================================================
// Rendering Tests (Full Pipeline)
// These tests exercise the full parse/style/layout/paint pipeline and run in CI.
// =============================================================================

#[test]
fn test_render_html_simple() {
  let mut renderer = deterministic_renderer();
  let result = renderer.render_html("<div>Hello World</div>", 64, 64);
  assert!(result.is_ok(), "Simple HTML should render successfully");

  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 64);
  assert_eq!(pixmap.height(), 64);
  let png_bytes = pixmap.encode_png().expect("encode PNG");
  assert_png_header(&png_bytes);
}

#[test]
fn test_render_html_with_style() {
  let mut renderer = deterministic_renderer();
  let html = r#"
        <html>
            <head>
                <style>
                    body { margin: 0; padding: 0; background: white; }
                    .box { width: 50px; height: 50px; background: blue; }
                </style>
            </head>
            <body>
                <div class="box"></div>
            </body>
        </html>
    "#;

  let result = renderer.render_html(html, 128, 128);
  assert!(result.is_ok(), "HTML with CSS should render successfully");

  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 128);
  assert_eq!(pixmap.height(), 128);
}

#[test]
fn test_render_html_various_sizes() {
  let mut renderer = deterministic_renderer();
  let html = "<div>Test</div>";

  // Small size
  let result = renderer.render_html(html, 10, 10);
  assert!(result.is_ok());
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 10);
  assert_eq!(pixmap.height(), 10);

  // Medium size
  let result = renderer.render_html(html, 80, 60);
  assert!(result.is_ok());
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 80);
  assert_eq!(pixmap.height(), 60);

  // Large size
  let result = renderer.render_html(html, 160, 120);
  assert!(result.is_ok());
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 160);
  assert_eq!(pixmap.height(), 120);
}

#[test]
fn test_render_html_with_background() {
  let mut renderer = deterministic_renderer();
  let html = "<div>Test</div>";

  let result = renderer.render_html_with_background(html, 64, 64, Rgba::rgb(255, 0, 0));
  assert!(result.is_ok());
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 64);
  assert_eq!(pixmap.height(), 64);

  // Background color should be restored
  assert_eq!(renderer.background_color(), Rgba::WHITE);
}

#[test]
fn test_layout_document() {
  let mut renderer = deterministic_renderer();
  let dom = renderer.parse_html("<div>Content</div>").unwrap();
  let result = renderer.layout_document(&dom, 200, 150);

  assert!(result.is_ok(), "Layout should succeed");

  let fragment_tree = result.unwrap();
  assert!(
    fragment_tree.fragment_count() > 0,
    "Fragment tree should have fragments"
  );
  assert_eq!(fragment_tree.viewport_size().width, 200.0);
  assert_eq!(fragment_tree.viewport_size().height, 150.0);
}

#[test]
fn test_layout_complex_document() {
  let mut renderer = deterministic_renderer();
  let html = r#"
        <html>
            <body>
                <header>Header</header>
                <main>
                    <article>
                        <h1>Title</h1>
                        <p>Paragraph 1</p>
                        <p>Paragraph 2</p>
                    </article>
                    <aside>Sidebar</aside>
                </main>
                <footer>Footer</footer>
            </body>
        </html>
    "#;

  let dom = renderer.parse_html(html).unwrap();
  let result = renderer.layout_document(&dom, 240, 180);

  assert!(result.is_ok(), "Complex layout should succeed");
  let fragment_tree = result.unwrap();
  assert!(fragment_tree.fragment_count() > 0);
  assert_eq!(fragment_tree.viewport_size().width, 240.0);
  assert_eq!(fragment_tree.viewport_size().height, 180.0);
}

#[test]
fn test_paint() {
  let mut renderer = deterministic_renderer();
  let dom = renderer.parse_html("<div>Content</div>").unwrap();
  let fragment_tree = renderer.layout_document(&dom, 200, 150).unwrap();

  let result = renderer.paint(&fragment_tree, 200, 150);
  assert!(result.is_ok(), "Paint should succeed");

  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 200);
  assert_eq!(pixmap.height(), 150);
  let png_bytes = pixmap.encode_png().expect("encode PNG");
  assert_png_header(&png_bytes);
}

#[test]
fn test_paint_different_size_than_layout() {
  let mut renderer = deterministic_renderer();
  let dom = renderer.parse_html("<div>Content</div>").unwrap();

  // Layout at one size
  let fragment_tree = renderer.layout_document(&dom, 200, 150).unwrap();

  // Paint at different size (should use paint dimensions)
  let result = renderer.paint(&fragment_tree, 100, 80);
  assert!(result.is_ok());

  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 100);
  assert_eq!(pixmap.height(), 80);
}

#[test]
fn test_end_to_end_simple() {
  let mut renderer = deterministic_renderer();

  let html = "<h1>Hello, FastRender!</h1>";
  let pixmap = renderer.render_html(html, 200, 150).unwrap();

  assert_eq!(pixmap.width(), 200);
  assert_eq!(pixmap.height(), 150);
  assert!(!pixmap.data().is_empty(), "Pixmap should have data");
}

#[test]
fn test_end_to_end_styled() {
  let mut renderer = deterministic_renderer();

  let html = r#"
        <!DOCTYPE html>
        <html>
            <head>
                <style>
                    body {
                        font-family: sans-serif;
                        background: #f0f0f0;
                        padding: 20px;
                    }
                    h1 {
                        color: navy;
                        font-size: 24px;
                    }
                    p {
                        color: #333;
                        line-height: 1.5;
                    }
                </style>
            </head>
            <body>
                <h1>Welcome to FastRender</h1>
                <p>This is a test paragraph with some text.</p>
                <p>This is another paragraph.</p>
            </body>
        </html>
    "#;

  let pixmap = renderer.render_html(html, 240, 180).unwrap();

  assert_eq!(pixmap.width(), 240);
  assert_eq!(pixmap.height(), 180);
}

#[test]
fn test_end_to_end_multiple_renders() {
  let mut renderer = deterministic_renderer();

  // Render multiple documents with the same renderer
  for i in 0..3 {
    let html = format!("<div>Render #{}</div>", i);
    let result = renderer.render_html(&html, 120, 60);
    assert!(result.is_ok(), "Render {} should succeed", i);
    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 120);
    assert_eq!(pixmap.height(), 60);
  }
}

#[test]
fn test_end_to_end_step_by_step() {
  let mut renderer = deterministic_renderer();

  // Step 1: Parse
  let html = "<div style='width: 100px; height: 100px; background: red;'>Box</div>";
  let dom = renderer.parse_html(html).unwrap();

  // Step 2: Layout
  let fragment_tree = renderer.layout_document(&dom, 200, 150).unwrap();
  assert!(fragment_tree.fragment_count() > 0);

  // Step 3: Paint
  let pixmap = renderer.paint(&fragment_tree, 200, 150).unwrap();
  assert_eq!(pixmap.width(), 200);
  assert_eq!(pixmap.height(), 150);
  let png_bytes = pixmap.encode_png().expect("encode PNG");
  assert_png_header(&png_bytes);
}

#[test]
fn test_render_with_flexbox() {
  let mut renderer = deterministic_renderer();
  let html = r#"
        <html>
            <head>
                <style>
                    .flex-container {
                        display: flex;
                        gap: 10px;
                    }
                    .flex-item {
                        width: 50px;
                        height: 50px;
                        background: blue;
                    }
                </style>
            </head>
            <body>
                <div class="flex-container">
                    <div class="flex-item">1</div>
                    <div class="flex-item">2</div>
                    <div class="flex-item">3</div>
                </div>
            </body>
        </html>
    "#;

  let result = renderer.render_html(html, 240, 120);
  assert!(result.is_ok(), "Flexbox layout should succeed");
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 240);
  assert_eq!(pixmap.height(), 120);
}

#[test]
fn test_render_with_grid() {
  let mut renderer = deterministic_renderer();
  let html = r#"
        <html>
            <head>
                <style>
                    .grid-container {
                        display: grid;
                        grid-template-columns: repeat(3, 1fr);
                        gap: 10px;
                    }
                    .grid-item {
                        background: green;
                        padding: 10px;
                    }
                </style>
            </head>
            <body>
                <div class="grid-container">
                    <div class="grid-item">1</div>
                    <div class="grid-item">2</div>
                    <div class="grid-item">3</div>
                </div>
            </body>
        </html>
    "#;

  let result = renderer.render_html(html, 240, 120);
  assert!(result.is_ok(), "Grid layout should succeed");
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 240);
  assert_eq!(pixmap.height(), 120);
}

#[test]
fn test_render_with_table() {
  let mut renderer = deterministic_renderer();
  let html = r#"
        <html>
            <body>
                <table style="width: 100%; border-collapse: collapse;">
                    <tr>
                        <th style="border: 1px solid black; padding: 8px;">Header 1</th>
                        <th style="border: 1px solid black; padding: 8px;">Header 2</th>
                    </tr>
                    <tr>
                        <td style="border: 1px solid black; padding: 8px;">Cell 1</td>
                        <td style="border: 1px solid black; padding: 8px;">Cell 2</td>
                    </tr>
                </table>
            </body>
        </html>
    "#;

  let result = renderer.render_html(html, 240, 160);
  assert!(result.is_ok(), "Table layout should succeed");
  let pixmap = result.unwrap();
  assert_eq!(pixmap.width(), 240);
  assert_eq!(pixmap.height(), 160);
}

#[test]
fn test_fragment_tree_reexport_with_layout() {
  // FragmentTree should be accessible and usable with layout
  use fastrender::FragmentTree;

  let mut renderer = deterministic_renderer();
  let dom = renderer.parse_html("<div>Test</div>").unwrap();
  let fragment_tree: FragmentTree = renderer.layout_document(&dom, 200, 150).unwrap();
  assert!(fragment_tree.fragment_count() > 0);
}
}
