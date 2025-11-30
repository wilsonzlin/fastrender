//! Public API tests for FastRender
//!
//! Tests for the FastRender public API (W6.T03).
//! These tests verify that the public API works correctly and covers
//! all major use cases.
//!
//! Note: Tests that require the full rendering pipeline are marked with
//! #[ignore] as the pipeline integration is pending (matching the pattern
//! in integration_test.rs).

use fastrender::api::{FastRender, FastRenderConfig};
use fastrender::css::Color;

// =============================================================================
// FastRender Creation Tests
// =============================================================================

#[test]
fn test_fastrender_new() {
    // Should be able to create a new FastRender instance
    let result = FastRender::new();
    assert!(result.is_ok(), "FastRender::new() should succeed");
}

#[test]
fn test_fastrender_with_default_config() {
    let config = FastRenderConfig::default();
    let result = FastRender::with_config(config);
    assert!(result.is_ok(), "FastRender::with_config(default) should succeed");
}

#[test]
fn test_fastrender_with_custom_config() {
    let config = FastRenderConfig::new()
        .with_default_background(Color::rgb(240, 240, 240))
        .with_default_viewport(1920, 1080);

    let result = FastRender::with_config(config);
    assert!(result.is_ok(), "FastRender::with_config(custom) should succeed");

    let renderer = result.unwrap();
    assert_eq!(renderer.background_color().r, 240);
    assert_eq!(renderer.background_color().g, 240);
    assert_eq!(renderer.background_color().b, 240);
}

// =============================================================================
// Configuration Tests
// =============================================================================

#[test]
fn test_config_default_values() {
    let config = FastRenderConfig::default();
    assert_eq!(config.background_color, Color::WHITE);
    assert_eq!(config.default_width, 800);
    assert_eq!(config.default_height, 600);
}

#[test]
fn test_config_builder_pattern() {
    let config = FastRenderConfig::new()
        .with_default_background(Color::rgb(100, 150, 200))
        .with_default_viewport(1024, 768);

    assert_eq!(config.background_color.r, 100);
    assert_eq!(config.background_color.g, 150);
    assert_eq!(config.background_color.b, 200);
    assert_eq!(config.default_width, 1024);
    assert_eq!(config.default_height, 768);
}

// =============================================================================
// HTML Parsing Tests
// =============================================================================

#[test]
fn test_parse_html_simple() {
    let renderer = FastRender::new().unwrap();
    let result = renderer.parse_html("<div>Hello</div>");
    assert!(result.is_ok(), "Simple HTML should parse successfully");
}

#[test]
fn test_parse_html_full_document() {
    let renderer = FastRender::new().unwrap();
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
    assert!(result.is_ok(), "Full HTML document should parse successfully");
}

#[test]
fn test_parse_html_with_style() {
    let renderer = FastRender::new().unwrap();
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
    assert!(result.is_ok(), "HTML with embedded CSS should parse successfully");
}

#[test]
fn test_parse_html_empty() {
    let renderer = FastRender::new().unwrap();
    let result = renderer.parse_html("");
    assert!(result.is_ok(), "Empty HTML should parse successfully");
}

// =============================================================================
// Component Access Tests
// =============================================================================

#[test]
fn test_font_context_access() {
    let renderer = FastRender::new().unwrap();
    let _font_context = renderer.font_context();
    // Just verify we can access it
}

#[test]
fn test_font_context_mut_access() {
    let mut renderer = FastRender::new().unwrap();
    let _font_context = renderer.font_context_mut();
    // Just verify we can access it
}

#[test]
fn test_layout_engine_access() {
    let renderer = FastRender::new().unwrap();
    let _layout_engine = renderer.layout_engine();
    // Just verify we can access it
}

#[test]
fn test_background_color_get_set() {
    let mut renderer = FastRender::new().unwrap();

    // Default is white
    assert_eq!(renderer.background_color(), Color::WHITE);

    // Set to custom color
    renderer.set_background_color(Color::rgb(50, 100, 150));
    assert_eq!(renderer.background_color().r, 50);
    assert_eq!(renderer.background_color().g, 100);
    assert_eq!(renderer.background_color().b, 150);
}

// =============================================================================
// Validation Tests
// =============================================================================

#[test]
fn test_render_html_invalid_dimensions() {
    let mut renderer = FastRender::new().unwrap();

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
    use fastrender::{FastRender, FastRenderConfig, Pixmap};

    let config = FastRenderConfig::new();
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
fn test_css_color_available() {
    // css::Color should be accessible for public API
    use fastrender::css::Color;
    let color = Color::rgb(100, 150, 200);
    assert_eq!(color.r, 100);
}

// =============================================================================
// Rendering Tests (Full Pipeline - Pending Integration)
// These tests require the full rendering pipeline to be integrated.
// They are marked with #[ignore] to match the pattern in integration_test.rs.
// =============================================================================

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_html_simple() {
    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_html("<div>Hello World</div>", 100, 100);
    assert!(result.is_ok(), "Simple HTML should render successfully");

    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 100);
    assert_eq!(pixmap.height(), 100);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_html_with_style() {
    let mut renderer = FastRender::new().unwrap();
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

    let result = renderer.render_html(html, 200, 200);
    assert!(result.is_ok(), "HTML with CSS should render successfully");

    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 200);
    assert_eq!(pixmap.height(), 200);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_html_various_sizes() {
    let mut renderer = FastRender::new().unwrap();
    let html = "<div>Test</div>";

    // Small size
    let result = renderer.render_html(html, 10, 10);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().width(), 10);

    // Medium size
    let result = renderer.render_html(html, 800, 600);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().width(), 800);

    // Large size
    let result = renderer.render_html(html, 1920, 1080);
    assert!(result.is_ok());
    assert_eq!(result.unwrap().width(), 1920);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_html_with_background() {
    let mut renderer = FastRender::new().unwrap();
    let html = "<div>Test</div>";

    let result = renderer.render_html_with_background(html, 100, 100, Color::rgb(255, 0, 0));
    assert!(result.is_ok());

    // Background color should be restored
    assert_eq!(renderer.background_color(), Color::WHITE);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_layout_document() {
    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html("<div>Content</div>").unwrap();
    let result = renderer.layout_document(&dom, 800, 600);

    assert!(result.is_ok(), "Layout should succeed");

    let fragment_tree = result.unwrap();
    assert!(fragment_tree.fragment_count() > 0, "Fragment tree should have fragments");
    assert_eq!(fragment_tree.viewport_size().width, 800.0);
    assert_eq!(fragment_tree.viewport_size().height, 600.0);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_layout_complex_document() {
    let mut renderer = FastRender::new().unwrap();
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
    let result = renderer.layout_document(&dom, 1024, 768);

    assert!(result.is_ok(), "Complex layout should succeed");
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_paint() {
    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html("<div>Content</div>").unwrap();
    let fragment_tree = renderer.layout_document(&dom, 800, 600).unwrap();

    let result = renderer.paint(&fragment_tree, 800, 600);
    assert!(result.is_ok(), "Paint should succeed");

    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 800);
    assert_eq!(pixmap.height(), 600);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_paint_different_size_than_layout() {
    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html("<div>Content</div>").unwrap();

    // Layout at one size
    let fragment_tree = renderer.layout_document(&dom, 800, 600).unwrap();

    // Paint at different size (should use paint dimensions)
    let result = renderer.paint(&fragment_tree, 400, 300);
    assert!(result.is_ok());

    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 400);
    assert_eq!(pixmap.height(), 300);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_end_to_end_simple() {
    let mut renderer = FastRender::new().unwrap();

    let html = "<h1>Hello, FastRender!</h1>";
    let pixmap = renderer.render_html(html, 800, 600).unwrap();

    assert_eq!(pixmap.width(), 800);
    assert_eq!(pixmap.height(), 600);
    assert!(!pixmap.data().is_empty(), "Pixmap should have data");
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_end_to_end_styled() {
    let mut renderer = FastRender::new().unwrap();

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

    let pixmap = renderer.render_html(html, 1024, 768).unwrap();

    assert_eq!(pixmap.width(), 1024);
    assert_eq!(pixmap.height(), 768);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_end_to_end_multiple_renders() {
    let mut renderer = FastRender::new().unwrap();

    // Render multiple documents with the same renderer
    for i in 0..5 {
        let html = format!("<div>Render #{}</div>", i);
        let result = renderer.render_html(&html, 200, 100);
        assert!(result.is_ok(), "Render {} should succeed", i);
    }
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_end_to_end_step_by_step() {
    let mut renderer = FastRender::new().unwrap();

    // Step 1: Parse
    let html = "<div style='width: 100px; height: 100px; background: red;'>Box</div>";
    let dom = renderer.parse_html(html).unwrap();

    // Step 2: Layout
    let fragment_tree = renderer.layout_document(&dom, 800, 600).unwrap();
    assert!(fragment_tree.fragment_count() > 0);

    // Step 3: Paint
    let pixmap = renderer.paint(&fragment_tree, 800, 600).unwrap();
    assert_eq!(pixmap.width(), 800);
    assert_eq!(pixmap.height(), 600);
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_with_flexbox() {
    let mut renderer = FastRender::new().unwrap();
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

    let result = renderer.render_html(html, 400, 200);
    assert!(result.is_ok(), "Flexbox layout should succeed");
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_with_grid() {
    let mut renderer = FastRender::new().unwrap();
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

    let result = renderer.render_html(html, 600, 400);
    assert!(result.is_ok(), "Grid layout should succeed");
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_render_with_table() {
    let mut renderer = FastRender::new().unwrap();
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

    let result = renderer.render_html(html, 500, 300);
    assert!(result.is_ok(), "Table layout should succeed");
}

#[test]
#[ignore = "Full rendering pipeline integration pending"]
fn test_fragment_tree_reexport_with_layout() {
    // FragmentTree should be accessible and usable with layout
    use fastrender::FragmentTree;

    let mut renderer = FastRender::new().unwrap();
    let dom = renderer.parse_html("<div>Test</div>").unwrap();
    let fragment_tree: FragmentTree = renderer.layout_document(&dom, 800, 600).unwrap();
    assert!(fragment_tree.fragment_count() > 0);
}
