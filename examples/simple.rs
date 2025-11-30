//! Simple example demonstrating the fastrender rendering pipeline.
//!
//! This example shows how to use the Renderer to convert HTML/CSS to images.
//!
//! # Running
//!
//! ```sh
//! cargo run --example simple
//! ```

use fastrender::Renderer;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Simple red box
    println!("Rendering example 1: Simple red box...");

    let html = r#"
        <html>
            <body style="margin: 20px;">
                <div style="width: 200px; height: 200px; background-color: red;"></div>
            </body>
        </html>
    "#;

    let renderer = Renderer::new();
    let png_bytes = renderer.render_to_png(html, 800, 600)?;
    fs::write("output_simple.png", &png_bytes)?;
    println!("  Wrote output_simple.png ({} bytes)", png_bytes.len());

    // Example 2: Styled text
    println!("Rendering example 2: Styled text...");

    let html_text = r#"
        <html>
            <head>
                <style>
                    body {
                        font-family: Arial, sans-serif;
                        background-color: #f0f0f0;
                        padding: 20px;
                    }
                    h1 {
                        color: navy;
                        font-size: 32px;
                    }
                    p {
                        color: #333;
                        font-size: 16px;
                        line-height: 1.5;
                    }
                </style>
            </head>
            <body>
                <h1>Hello, Fastrender!</h1>
                <p>This is an example of HTML to image rendering.</p>
                <p>The complete pipeline handles parsing, styling, layout, and paint.</p>
            </body>
        </html>
    "#;

    let png_bytes = renderer.render_to_png(html_text, 800, 400)?;
    fs::write("output_text.png", &png_bytes)?;
    println!("  Wrote output_text.png ({} bytes)", png_bytes.len());

    // Example 3: Flexbox layout
    println!("Rendering example 3: Flexbox layout...");

    let html_flex = r#"
        <html>
            <head>
                <style>
                    body {
                        margin: 0;
                        padding: 20px;
                        background-color: white;
                    }
                    .container {
                        display: flex;
                        gap: 20px;
                    }
                    .box {
                        width: 100px;
                        height: 100px;
                        display: flex;
                        align-items: center;
                        justify-content: center;
                        color: white;
                        font-weight: bold;
                    }
                    .red { background-color: #e74c3c; }
                    .green { background-color: #2ecc71; }
                    .blue { background-color: #3498db; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="box red">1</div>
                    <div class="box green">2</div>
                    <div class="box blue">3</div>
                </div>
            </body>
        </html>
    "#;

    let png_bytes = renderer.render_to_png(html_flex, 500, 200)?;
    fs::write("output_flexbox.png", &png_bytes)?;
    println!("  Wrote output_flexbox.png ({} bytes)", png_bytes.len());

    // Example 4: Grid layout
    println!("Rendering example 4: Grid layout...");

    let html_grid = r#"
        <html>
            <head>
                <style>
                    body {
                        margin: 0;
                        padding: 20px;
                        background-color: #ecf0f1;
                    }
                    .grid {
                        display: grid;
                        grid-template-columns: repeat(3, 1fr);
                        gap: 10px;
                    }
                    .item {
                        background-color: #9b59b6;
                        height: 80px;
                        border-radius: 8px;
                    }
                </style>
            </head>
            <body>
                <div class="grid">
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                    <div class="item"></div>
                </div>
            </body>
        </html>
    "#;

    let png_bytes = renderer.render_to_png(html_grid, 400, 250)?;
    fs::write("output_grid.png", &png_bytes)?;
    println!("  Wrote output_grid.png ({} bytes)", png_bytes.len());

    // Example 5: Using the builder pattern with custom viewport
    println!("Rendering example 5: Custom viewport with builder...");

    let renderer_custom = Renderer::builder()
        .viewport_size(1024, 768)
        .background_color(fastrender::css::Color::rgb(240, 240, 240))
        .build();

    let html_card = r#"
        <html>
            <head>
                <style>
                    body {
                        display: flex;
                        justify-content: center;
                        padding: 40px;
                    }
                    .card {
                        width: 300px;
                        padding: 20px;
                        background-color: white;
                        border-radius: 12px;
                        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
                    }
                    .card h2 {
                        margin: 0 0 10px 0;
                        color: #2c3e50;
                    }
                    .card p {
                        color: #7f8c8d;
                        margin: 0;
                    }
                </style>
            </head>
            <body>
                <div class="card">
                    <h2>Card Title</h2>
                    <p>This is a card component with shadow and rounded corners.</p>
                </div>
            </body>
        </html>
    "#;

    let png_bytes = renderer_custom.render_to_png(html_card, 500, 250)?;
    fs::write("output_card.png", &png_bytes)?;
    println!("  Wrote output_card.png ({} bytes)", png_bytes.len());

    // Example 6: Multiple output formats
    println!("Rendering example 6: Multiple output formats...");

    let html_formats = r#"
        <html>
            <body style="padding: 20px;">
                <div style="width: 200px; height: 200px;
                            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                            border-radius: 20px;">
                </div>
            </body>
        </html>
    "#;

    // PNG (lossless)
    let png = renderer.render_to_png(html_formats, 300, 300)?;
    fs::write("output_format.png", &png)?;
    println!("  Wrote output_format.png ({} bytes)", png.len());

    // JPEG (lossy, quality 85)
    let jpeg = renderer.render_to_jpeg(html_formats, 300, 300, 85)?;
    fs::write("output_format.jpg", &jpeg)?;
    println!("  Wrote output_format.jpg ({} bytes)", jpeg.len());

    // WebP (lossy, quality 85)
    let webp = renderer.render_to_webp(html_formats, 300, 300, 85)?;
    fs::write("output_format.webp", &webp)?;
    println!("  Wrote output_format.webp ({} bytes)", webp.len());

    println!("\nAll examples completed successfully!");

    Ok(())
}
