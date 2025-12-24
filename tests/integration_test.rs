//! Integration tests for the HTML to image renderer
//!
//! These tests verify the complete rendering pipeline from HTML/CSS to images.

use fastrender::{FastRender, FastRenderConfig};

fn with_large_stack<F, R>(f: F) -> R
where
  F: FnOnce() -> R + Send + 'static,
  R: Send + 'static,
{
  const STACK_SIZE: usize = 64 * 1024 * 1024;
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
fn test_simple_html_rendering() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <body>
                <div style="width: 100px; height: 100px; background-color: red;"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
    let image_bytes = result.unwrap();
    assert!(!image_bytes.is_empty());
  });
}

#[test]
fn test_text_rendering() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    body { font-family: sans-serif; }
                    h1 { color: blue; }
                </style>
            </head>
            <body>
                <h1>Hello World</h1>
                <p>This is a test paragraph.</p>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_flexbox_layout() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .container {
                        display: flex;
                        justify-content: space-between;
                    }
                    .box {
                        width: 100px;
                        height: 100px;
                        background-color: blue;
                    }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="box"></div>
                    <div class="box"></div>
                    <div class="box"></div>
                </div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_grid_layout() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .grid {
                        display: grid;
                        grid-template-columns: 1fr 1fr 1fr;
                        grid-gap: 10px;
                    }
                    .item {
                        background-color: green;
                        height: 50px;
                    }
                </style>
            </head>
            <body>
                <div class="grid">
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                </div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_gradient_background() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .gradient {
                        width: 400px;
                        height: 300px;
                        background-image: linear-gradient(to right, red, blue);
                    }
                </style>
            </head>
            <body>
                <div class="gradient"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_border_radius() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .rounded {
                        width: 200px;
                        height: 200px;
                        background-color: purple;
                        border-radius: 20px;
                    }
                </style>
            </head>
            <body>
                <div class="rounded"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_box_shadow() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .shadow {
                        width: 200px;
                        height: 100px;
                        background-color: white;
                        box-shadow: 5px 5px 10px rgba(0, 0, 0, 0.5);
                    }
                </style>
            </head>
            <body style="background-color: #f0f0f0;">
                <div class="shadow"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_transform() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .transformed {
                        width: 100px;
                        height: 100px;
                        background-color: orange;
                        transform: rotate(45deg);
                    }
                </style>
            </head>
            <body>
                <div class="transformed"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_multiple_output_formats() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <body>
                <div style="width: 100px; height: 100px; background-color: cyan;"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();

    // Test PNG
    let png_result = renderer.render_to_png(html, 400, 300);
    assert!(png_result.is_ok());

    // Test JPEG
    let jpeg_result = renderer.render_to_jpeg(html, 400, 300, 85);
    assert!(jpeg_result.is_ok());

    // Test WebP
    let webp_result = renderer.render_to_webp(html, 400, 300, 85);
    assert!(webp_result.is_ok());
  });
}

#[test]
fn test_custom_viewport_size() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <body>
                <div style="width: 50%; height: 50%; background-color: yellow;"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::builder()
      .viewport_size(1024, 768)
      .build()
      .unwrap();

    let result = renderer.render(html);
    assert!(result.is_ok());
  });
}

#[test]
fn test_invalid_dimensions() {
  with_large_stack(|| {
    let html = "<html><body></body></html>";
    let mut renderer = FastRender::new().unwrap();

    let result = renderer.render_to_png(html, 0, 0);
    assert!(result.is_err());
  });
}

#[test]
fn test_malformed_html() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <body>
                <div>Unclosed div
                <p>Some text
            </body>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    // Should still work due to error recovery
    assert!(result.is_ok());
  });
}

#[test]
fn test_class_selector() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .red-box {
                        width: 100px;
                        height: 100px;
                        background-color: red;
                    }
                </style>
            </head>
            <body>
                <div class="red-box"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_id_selector() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    #main {
                        width: 200px;
                        height: 150px;
                        background-color: green;
                    }
                </style>
            </head>
            <body>
                <div id="main"></div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn test_nested_elements() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <style>
                    .outer {
                        padding: 20px;
                        background-color: #f0f0f0;
                    }
                    .inner {
                        padding: 10px;
                        background-color: white;
                    }
                </style>
            </head>
            <body>
                <div class="outer">
                    <div class="inner">
                        <p>Nested content</p>
                    </div>
                </div>
            </body>
        </html>
    "#;

    let mut renderer = FastRender::new().unwrap();
    let result = renderer.render_to_png(html, 800, 600);

    assert!(result.is_ok());
  });
}

#[test]
fn meta_viewport_flips_media_query_when_enabled() {
  with_large_stack(|| {
    let html = r#"
        <html>
            <head>
                <meta name="viewport" content="width=device-width, initial-scale=2">
                <style>
                    html, body { margin: 0; padding: 0; width: 100%; height: 100%; }
                    body { background: rgb(200, 0, 0); }
                    @media (max-width: 500px) {
                        body { background: rgb(0, 180, 0); }
                    }
                </style>
            </head>
            <body></body>
        </html>
    "#;

    let mut disabled = FastRender::new().unwrap();
    let no_meta = disabled.render_html(html, 600, 400).unwrap();
    let no_meta_pixel = no_meta.pixel(0, 0).unwrap();
    assert!(no_meta_pixel.red() > no_meta_pixel.green());

    let mut enabled =
      FastRender::with_config(FastRenderConfig::new().with_meta_viewport(true)).unwrap();
    let meta_applied = enabled.render_html(html, 600, 400).unwrap();
    let meta_pixel = meta_applied.pixel(0, 0).unwrap();
    assert!(meta_pixel.green() > meta_pixel.red());
  });
}
