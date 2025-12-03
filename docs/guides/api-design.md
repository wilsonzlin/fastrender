# Phase 11: API Design - Public API Surface

**Duration:** Reference document (ongoing)
**Prerequisites:**
- Architecture decisions (00-architecture-decisions.md)
- Core implementation progress
- Migration guide context (07-migration-guide.md)
**Dependencies:**
- Core renderer implementation
- Error handling design
- Font system design
- Output format support
**Output:** Complete public API specification for FastRender library and CLI

## Objectives

This document defines the **complete public API surface** for FastRender:

- **High-level design principles** - Simplicity, safety, performance, ergonomics
- **Core public types** - `Renderer`, `RenderOptions`, `Image`, etc.
- **Complete API surface** - Every public function and type
- **Error handling** - `Result` types, error categories, error messages
- **Configuration options** - Viewport, DPI, fonts, output formats
- **Async API** - Optional async rendering for integration
- **CLI tool design** - `fastrender` command-line interface
- **Library usage examples** - Common use cases with complete code
- **API stability guarantees** - Semver policy and backwards compatibility
- **Internal vs external APIs** - What to expose, what to keep private
- **Documentation requirements** - Rustdoc standards and examples

This is **critical for usability** - a great engine with a bad API won't be adopted.

## Context

### Why API Design Matters

**A rendering engine is a library**, not an application. Users interact with FastRender through:
1. **Rust library API** - Core use case
2. **CLI tool** - For command-line usage
3. **Language bindings** - Future Python, Node.js, etc. bindings

**Good API design:**
- Makes simple things simple
- Makes complex things possible
- Prevents misuse
- Evolves gracefully
- Self-documents through types

**Bad API design:**
- Requires reading docs for basic usage
- Forces boilerplate code
- Allows easy mistakes
- Breaks between versions
- Requires runtime checking

### The Problem V1 Has

V1's API has issues:

```rust
// V1 API - unclear what dimensions are used
let renderer = Renderer::new();
let image = renderer.render_html(html)?;  // What size?

// V1 API - hard to configure
// No way to set fonts, DPI, or viewport
```

**Problems:**
- Implicit defaults (viewport size unclear)
- No font configuration
- Limited error information
- No output format choice
- Hard to extend

### V2 API Goals

**Goals:**
1. **Explicit over implicit** - No surprising defaults
2. **Type-safe** - Compiler catches mistakes
3. **Discoverable** - IDE autocomplete shows options
4. **Flexible** - Support simple and complex use cases
5. **Documented** - Every public item has rustdoc
6. **Stable** - Follow semver strictly

---

# High-Level API Design Principles

## Principle 1: Simplicity First

**Simple things should be simple:**

```rust
// Good: Simple case requires minimal code
use fastrender::Renderer;

let renderer = Renderer::new();
let image = renderer.render(html, 800, 600)?;
image.save("output.png")?;
```

**Complex things should be possible:**

```rust
// Good: Complex case is verbose but flexible
use fastrender::{Renderer, RenderOptions, FontConfig, OutputFormat};

let font_config = FontConfig::new()
    .add_font_dir("/usr/share/fonts")
    .add_fallback("DejaVu Sans");

let options = RenderOptions {
    viewport_width: 1920,
    viewport_height: 1080,
    device_pixel_ratio: 2.0,
    default_font_size: 16.0,
    default_font_family: vec!["Arial".into()],
    ..Default::default()
};

let renderer = Renderer::with_options(options)
    .font_config(font_config)
    .build()?;

let image = renderer.render(html, 1920, 1080)?;
image.save_as("output.jpg", OutputFormat::Jpeg { quality: 90 })?;
```

## Principle 2: Type Safety

**Use types to prevent errors:**

```rust
// Bad: Easy to mix up width and height
fn render(width: u32, height: u32) -> Result<Image>;

// Better: Named fields
fn render(size: ViewportSize) -> Result<Image>;

struct ViewportSize {
    width: u32,
    height: u32,
}

// Best: Builder pattern with clear names
Renderer::new()
    .viewport_size(800, 600)
    .render(html)?
```

**Use enums for variants:**

```rust
// Good: All output formats in one type
pub enum OutputFormat {
    Png,
    Jpeg { quality: u8 },
    WebP { quality: u8, lossless: bool },
}
```

## Principle 3: Builder Pattern for Configuration

**Configuration uses builders:**

```rust
// Good: Builder pattern for options
let renderer = Renderer::builder()
    .viewport_size(800, 600)
    .device_pixel_ratio(2.0)
    .default_font_size(16.0)
    .font_config(font_config)
    .build()?;
```

**Why:**
- Readable
- Extensible (add new options without breaking)
- Self-documenting
- Type-safe

## Principle 4: Explicit Errors

**All errors are typed:**

```rust
// Good: Detailed error types
pub enum RenderError {
    HtmlParse { source: html5ever::ParseError },
    CssParse { source: cssparser::ParseError },
    Layout { message: String },
    Font { message: String },
    Raster { message: String },
    Io { source: std::io::Error },
}

// Implements std::error::Error
impl std::error::Error for RenderError { }
```

**Error messages are helpful:**

```rust
// Bad
Err(RenderError::Parse("error"))

// Good
Err(RenderError::HtmlParse {
    source: err,
    context: "Failed to parse HTML at line 5: unclosed <div> tag",
})
```

## Principle 5: Zero-Cost Abstractions

**No runtime overhead for type safety:**

```rust
// Good: newtype wrapper with no runtime cost
#[repr(transparent)]
pub struct Pixels(f32);

#[repr(transparent)]
pub struct Em(f32);

// Prevents mixing up units
fn set_font_size(size: Em) { }
fn set_width(width: Pixels) { }

// Compile error:
set_font_size(Pixels(12.0));  // ❌ Won't compile
```

## Principle 6: Async-Ready (but Optional)

**Sync API is primary:**

```rust
// Primary API is synchronous
let image = renderer.render(html, 800, 600)?;
```

**Async API is available:**

```rust
// Async API for integration with async runtimes
let image = renderer.render_async(html, 800, 600).await?;
```

**Why:**
- Most use cases are synchronous (CLI tools, scripts)
- Async adds complexity
- Both APIs share implementation

---

# Core Public Types

## `Renderer`

**The main rendering engine.**

```rust
/// HTML/CSS rendering engine
///
/// # Examples
///
/// ```
/// use fastrender::Renderer;
///
/// let renderer = Renderer::new();
/// let image = renderer.render("<h1>Hello</h1>", 800, 600)?;
/// image.save("output.png")?;
/// ```
pub struct Renderer {
    // Internal fields (private)
}

impl Renderer {
    /// Create a new renderer with default options
    pub fn new() -> Self;

    /// Create a builder for custom configuration
    pub fn builder() -> RendererBuilder;

    /// Render HTML to an image
    pub fn render(
        &self,
        html: &str,
        width: u32,
        height: u32,
    ) -> Result<Image, RenderError>;

    /// Render HTML to an image (async)
    #[cfg(feature = "async")]
    pub async fn render_async(
        &self,
        html: &str,
        width: u32,
        height: u32,
    ) -> Result<Image, RenderError>;

    /// Render HTML to a file directly
    pub fn render_to_file(
        &self,
        html: &str,
        width: u32,
        height: u32,
        path: impl AsRef<Path>,
    ) -> Result<(), RenderError>;

    /// Render HTML with CSS from separate string
    pub fn render_with_css(
        &self,
        html: &str,
        css: &str,
        width: u32,
        height: u32,
    ) -> Result<Image, RenderError>;
}
```

## `RendererBuilder`

**Builder for `Renderer` configuration.**

```rust
/// Builder for configuring a Renderer
///
/// # Examples
///
/// ```
/// use fastrender::{Renderer, FontConfig};
///
/// let font_config = FontConfig::system_fonts();
///
/// let renderer = Renderer::builder()
///     .viewport_size(1920, 1080)
///     .device_pixel_ratio(2.0)
///     .default_font_size(16.0)
///     .font_config(font_config)
///     .build()?;
/// ```
pub struct RendererBuilder {
    // Internal fields (private)
}

impl RendererBuilder {
    /// Set viewport dimensions (default: required in render())
    pub fn viewport_size(mut self, width: u32, height: u32) -> Self;

    /// Set device pixel ratio (default: 1.0)
    pub fn device_pixel_ratio(mut self, ratio: f32) -> Self;

    /// Set default font size in pixels (default: 16.0)
    pub fn default_font_size(mut self, size: f32) -> Self;

    /// Set default font family (default: ["serif"])
    pub fn default_font_family(mut self, families: Vec<String>) -> Self;

    /// Set font configuration
    pub fn font_config(mut self, config: FontConfig) -> Self;

    /// Enable or disable JavaScript (default: false)
    /// Note: JS execution not yet implemented
    pub fn enable_javascript(mut self, enable: bool) -> Self;

    /// Set user agent string
    pub fn user_agent(mut self, ua: String) -> Self;

    /// Build the renderer
    pub fn build(self) -> Result<Renderer, RenderError>;
}

impl Default for RendererBuilder {
    fn default() -> Self;
}
```

## `RenderOptions`

**Configuration for a single render.**

```rust
/// Options for a single render operation
#[derive(Debug, Clone)]
pub struct RenderOptions {
    /// Viewport width in CSS pixels
    pub viewport_width: u32,

    /// Viewport height in CSS pixels
    pub viewport_height: u32,

    /// Device pixel ratio (1.0 = standard, 2.0 = retina)
    pub device_pixel_ratio: f32,

    /// Default font size in pixels
    pub default_font_size: f32,

    /// Default font family
    pub default_font_family: Vec<String>,

    /// Media type (screen, print)
    pub media_type: MediaType,

    /// Background color (if not specified in CSS)
    pub default_background: Color,
}

impl Default for RenderOptions {
    fn default() -> Self {
        Self {
            viewport_width: 800,
            viewport_height: 600,
            device_pixel_ratio: 1.0,
            default_font_size: 16.0,
            default_font_family: vec!["serif".into()],
            media_type: MediaType::Screen,
            default_background: Color::WHITE,
        }
    }
}
```

## `Image`

**Rendered output image.**

```rust
/// A rendered image
///
/// Contains pixel data from rendering HTML/CSS.
pub struct Image {
    // Internal: tiny_skia::Pixmap or similar
}

impl Image {
    /// Get image width in pixels
    pub fn width(&self) -> u32;

    /// Get image height in pixels
    pub fn height(&self) -> u32;

    /// Get pixel data as RGBA bytes
    pub fn as_rgba(&self) -> &[u8];

    /// Get pixel data as mutable RGBA bytes
    pub fn as_rgba_mut(&mut self) -> &mut [u8];

    /// Get a single pixel
    pub fn get_pixel(&self, x: u32, y: u32) -> Color;

    /// Set a single pixel
    pub fn set_pixel(&mut self, x: u32, y: u32, color: Color);

    /// Save to file (format inferred from extension)
    pub fn save(&self, path: impl AsRef<Path>) -> Result<(), RenderError>;

    /// Save with explicit format
    pub fn save_as(
        &self,
        path: impl AsRef<Path>,
        format: OutputFormat,
    ) -> Result<(), RenderError>;

    /// Encode to PNG bytes
    pub fn encode_png(&self) -> Result<Vec<u8>, RenderError>;

    /// Encode to JPEG bytes
    pub fn encode_jpeg(&self, quality: u8) -> Result<Vec<u8>, RenderError>;

    /// Encode to WebP bytes
    pub fn encode_webp(&self, quality: u8, lossless: bool) -> Result<Vec<u8>, RenderError>;

    /// Encode with specified format
    pub fn encode(&self, format: OutputFormat) -> Result<Vec<u8>, RenderError>;

    /// Resize image
    pub fn resize(&self, width: u32, height: u32) -> Image;

    /// Crop image
    pub fn crop(&self, x: u32, y: u32, width: u32, height: u32) -> Image;
}
```

## `FontConfig`

**Font loading and fallback configuration.**

```rust
/// Font configuration
///
/// Specifies where to find fonts and fallback chain.
///
/// # Examples
///
/// ```
/// use fastrender::FontConfig;
///
/// // Use system fonts
/// let config = FontConfig::system_fonts();
///
/// // Custom fonts
/// let config = FontConfig::new()
///     .add_font_file("fonts/CustomFont.ttf")
///     .add_font_dir("/usr/share/fonts")
///     .add_fallback("DejaVu Sans")
///     .add_fallback("Arial");
/// ```
pub struct FontConfig {
    // Internal fields (private)
}

impl FontConfig {
    /// Create empty font configuration
    pub fn new() -> Self;

    /// Use system fonts (platform-specific directories)
    pub fn system_fonts() -> Self;

    /// Add a font file
    pub fn add_font_file(mut self, path: impl AsRef<Path>) -> Self;

    /// Add a directory of fonts
    pub fn add_font_dir(mut self, path: impl AsRef<Path>) -> Self;

    /// Add a fallback font by name
    pub fn add_fallback(mut self, name: impl Into<String>) -> Self;

    /// Set font cache size in MB (default: 100)
    pub fn cache_size(mut self, mb: usize) -> Self;
}

impl Default for FontConfig {
    fn default() -> Self {
        Self::system_fonts()
    }
}
```

## `Color`

**RGBA color value.**

```rust
/// RGBA color
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

impl Color {
    /// Create from RGBA components
    pub const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Self;

    /// Create from RGB components (alpha = 255)
    pub const fn rgb(r: u8, g: u8, b: u8) -> Self;

    /// Create from hex string (#RGB, #RRGGBB, #RRGGBBAA)
    pub fn from_hex(hex: &str) -> Result<Self, ParseError>;

    /// Convert to hex string
    pub fn to_hex(&self) -> String;

    /// Common colors
    pub const WHITE: Color = Color::rgb(255, 255, 255);
    pub const BLACK: Color = Color::rgb(0, 0, 0);
    pub const RED: Color = Color::rgb(255, 0, 0);
    pub const GREEN: Color = Color::rgb(0, 255, 0);
    pub const BLUE: Color = Color::rgb(0, 0, 255);
    pub const TRANSPARENT: Color = Color::rgba(0, 0, 0, 0);
}

impl FromStr for Color {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err>;
}
```

## `OutputFormat`

**Output image format.**

```rust
/// Image output format
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OutputFormat {
    /// PNG (lossless)
    Png,

    /// JPEG (lossy)
    Jpeg {
        /// Quality 0-100
        quality: u8,
    },

    /// WebP
    WebP {
        /// Quality 0-100
        quality: u8,
        /// Use lossless compression
        lossless: bool,
    },
}

impl OutputFormat {
    /// Infer format from file extension
    pub fn from_path(path: impl AsRef<Path>) -> Option<Self>;
}
```

## `MediaType`

**CSS media type.**

```rust
/// CSS media type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MediaType {
    /// Screen media (default)
    Screen,
    /// Print media
    Print,
    /// All media
    All,
}

impl FromStr for MediaType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err>;
}
```

---

# Error Handling API

## `RenderError`

**Main error type.**

```rust
/// Rendering errors
#[derive(Debug, thiserror::Error)]
pub enum RenderError {
    /// HTML parsing failed
    #[error("HTML parse error: {source}")]
    HtmlParse {
        #[from]
        source: html5ever::ParseError,
    },

    /// CSS parsing failed
    #[error("CSS parse error: {source}")]
    CssParse {
        #[from]
        source: cssparser::ParseError,
    },

    /// Layout error
    #[error("Layout error: {message}")]
    Layout {
        message: String,
    },

    /// Font loading or shaping error
    #[error("Font error: {message}")]
    Font {
        message: String,
    },

    /// Rasterization error
    #[error("Raster error: {message}")]
    Raster {
        message: String,
    },

    /// I/O error
    #[error("I/O error: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },

    /// Invalid configuration
    #[error("Invalid configuration: {message}")]
    Config {
        message: String,
    },
}

pub type Result<T> = std::result::Result<T, RenderError>;
```

## Error Categories

**Errors are categorized for handling:**

```rust
impl RenderError {
    /// Check if error is recoverable
    pub fn is_recoverable(&self) -> bool {
        match self {
            RenderError::HtmlParse { .. } => true,  // Can render partial HTML
            RenderError::CssParse { .. } => true,   // Can skip invalid CSS
            RenderError::Layout { .. } => false,    // Fatal
            RenderError::Font { .. } => true,       // Can fall back
            RenderError::Raster { .. } => false,    // Fatal
            RenderError::Io { .. } => false,        // Fatal
            RenderError::Config { .. } => false,    // Fatal
        }
    }

    /// Get error severity
    pub fn severity(&self) -> ErrorSeverity {
        match self {
            RenderError::HtmlParse { .. } => ErrorSeverity::Warning,
            RenderError::CssParse { .. } => ErrorSeverity::Warning,
            RenderError::Layout { .. } => ErrorSeverity::Error,
            RenderError::Font { .. } => ErrorSeverity::Warning,
            RenderError::Raster { .. } => ErrorSeverity::Error,
            RenderError::Io { .. } => ErrorSeverity::Error,
            RenderError::Config { .. } => ErrorSeverity::Error,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    Warning,
    Error,
}
```

---

# Complete Public API Reference

## Module Structure

```
fastrender/
├── lib.rs                    # Re-exports public API
├── renderer.rs               # Renderer, RendererBuilder
├── image.rs                  # Image
├── font.rs                   # FontConfig
├── error.rs                  # RenderError
├── color.rs                  # Color
├── options.rs                # RenderOptions, OutputFormat, MediaType
└── async_renderer.rs         # Async API (feature-gated)
```

## Public API Surface

### Top-Level Re-exports

```rust
// src/lib.rs

// Core types
pub use renderer::{Renderer, RendererBuilder};
pub use image::Image;
pub use font::FontConfig;
pub use color::Color;
pub use error::{RenderError, Result};
pub use options::{RenderOptions, OutputFormat, MediaType};

// Async API (optional)
#[cfg(feature = "async")]
pub use async_renderer::AsyncRenderer;

// Version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
```

### Complete Public Functions

**Renderer:**
- `Renderer::new() -> Self`
- `Renderer::builder() -> RendererBuilder`
- `Renderer::render(&self, html: &str, width: u32, height: u32) -> Result<Image>`
- `Renderer::render_async(&self, html: &str, width: u32, height: u32) -> impl Future<Output = Result<Image>>`
- `Renderer::render_to_file(&self, html: &str, width: u32, height: u32, path: impl AsRef<Path>) -> Result<()>`
- `Renderer::render_with_css(&self, html: &str, css: &str, width: u32, height: u32) -> Result<Image>`

**RendererBuilder:**
- `RendererBuilder::new() -> Self`
- `RendererBuilder::viewport_size(self, width: u32, height: u32) -> Self`
- `RendererBuilder::device_pixel_ratio(self, ratio: f32) -> Self`
- `RendererBuilder::default_font_size(self, size: f32) -> Self`
- `RendererBuilder::default_font_family(self, families: Vec<String>) -> Self`
- `RendererBuilder::font_config(self, config: FontConfig) -> Self`
- `RendererBuilder::enable_javascript(self, enable: bool) -> Self`
- `RendererBuilder::user_agent(self, ua: String) -> Self`
- `RendererBuilder::build(self) -> Result<Renderer>`

**Image:**
- `Image::width(&self) -> u32`
- `Image::height(&self) -> u32`
- `Image::as_rgba(&self) -> &[u8]`
- `Image::as_rgba_mut(&mut self) -> &mut [u8]`
- `Image::get_pixel(&self, x: u32, y: u32) -> Color`
- `Image::set_pixel(&mut self, x: u32, y: u32, color: Color)`
- `Image::save(&self, path: impl AsRef<Path>) -> Result<()>`
- `Image::save_as(&self, path: impl AsRef<Path>, format: OutputFormat) -> Result<()>`
- `Image::encode_png(&self) -> Result<Vec<u8>>`
- `Image::encode_jpeg(&self, quality: u8) -> Result<Vec<u8>>`
- `Image::encode_webp(&self, quality: u8, lossless: bool) -> Result<Vec<u8>>`
- `Image::encode(&self, format: OutputFormat) -> Result<Vec<u8>>`
- `Image::resize(&self, width: u32, height: u32) -> Image`
- `Image::crop(&self, x: u32, y: u32, width: u32, height: u32) -> Image`

**FontConfig:**
- `FontConfig::new() -> Self`
- `FontConfig::system_fonts() -> Self`
- `FontConfig::add_font_file(self, path: impl AsRef<Path>) -> Self`
- `FontConfig::add_font_dir(self, path: impl AsRef<Path>) -> Self`
- `FontConfig::add_fallback(self, name: impl Into<String>) -> Self`
- `FontConfig::cache_size(self, mb: usize) -> Self`

**Color:**
- `Color::rgba(r: u8, g: u8, b: u8, a: u8) -> Self`
- `Color::rgb(r: u8, g: u8, b: u8) -> Self`
- `Color::from_hex(hex: &str) -> Result<Self>`
- `Color::to_hex(&self) -> String`

---

# Async API Considerations

## Async Feature Flag

```toml
[features]
default = []
async = ["tokio", "async-trait"]
```

## Async Renderer

```rust
#[cfg(feature = "async")]
pub struct AsyncRenderer {
    inner: Renderer,
    runtime: tokio::runtime::Runtime,
}

#[cfg(feature = "async")]
impl AsyncRenderer {
    /// Create async renderer
    pub fn new() -> Self;

    /// Render HTML asynchronously
    pub async fn render(
        &self,
        html: &str,
        width: u32,
        height: u32,
    ) -> Result<Image>;

    /// Render multiple pages concurrently
    pub async fn render_batch(
        &self,
        pages: Vec<(&str, u32, u32)>,
    ) -> Vec<Result<Image>>;
}
```

**Use cases:**
- Web servers (Axum, Actix)
- Background job processing
- Parallel rendering

---

# CLI Tool Design

## Command Structure

```bash
# Basic usage
fastrender input.html output.png

# Specify dimensions
fastrender input.html output.png --width 1920 --height 1080

# Custom DPI
fastrender input.html output.png --dpi 2.0

# JPEG output
fastrender input.html output.jpg --quality 90

# Separate CSS
fastrender input.html output.png --css styles.css

# From stdin
echo "<h1>Hello</h1>" | fastrender - output.png
```

## CLI Implementation

**File: `src/bin/fastrender.rs`**

```rust
use clap::Parser;
use fastrender::{Renderer, FontConfig, OutputFormat};
use std::path::PathBuf;

#[derive(Parser)]
#[clap(name = "fastrender")]
#[clap(about = "Render HTML/CSS to images")]
#[clap(version)]
struct Cli {
    /// Input HTML file (use '-' for stdin)
    #[clap(value_parser)]
    input: PathBuf,

    /// Output image file
    #[clap(value_parser)]
    output: PathBuf,

    /// Width in pixels
    #[clap(short, long, default_value = "800")]
    width: u32,

    /// Height in pixels
    #[clap(short = 'H', long, default_value = "600")]
    height: u32,

    /// Device pixel ratio (DPI)
    #[clap(long, default_value = "1.0")]
    dpi: f32,

    /// External CSS file
    #[clap(short, long)]
    css: Option<PathBuf>,

    /// Font directory
    #[clap(long)]
    font_dir: Option<PathBuf>,

    /// JPEG quality (1-100)
    #[clap(short, long)]
    quality: Option<u8>,

    /// Verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // Read HTML
    let html = if cli.input.to_str() == Some("-") {
        use std::io::Read;
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        buffer
    } else {
        std::fs::read_to_string(&cli.input)?
    };

    // Read CSS if provided
    let css = if let Some(css_path) = cli.css {
        Some(std::fs::read_to_string(css_path)?)
    } else {
        None
    };

    // Build font config
    let mut font_config = FontConfig::system_fonts();
    if let Some(font_dir) = cli.font_dir {
        font_config = font_config.add_font_dir(font_dir);
    }

    // Create renderer
    let renderer = Renderer::builder()
        .device_pixel_ratio(cli.dpi)
        .font_config(font_config)
        .build()?;

    if cli.verbose {
        eprintln!("Rendering {}x{} ({}x DPI)...", cli.width, cli.height, cli.dpi);
    }

    // Render
    let image = if let Some(css) = css {
        renderer.render_with_css(&html, &css, cli.width, cli.height)?
    } else {
        renderer.render(&html, cli.width, cli.height)?
    };

    // Determine output format
    let format = if let Some(quality) = cli.quality {
        OutputFormat::Jpeg { quality }
    } else {
        OutputFormat::from_path(&cli.output)
            .unwrap_or(OutputFormat::Png)
    };

    // Save
    image.save_as(&cli.output, format)?;

    if cli.verbose {
        eprintln!("Saved to {}", cli.output.display());
    }

    Ok(())
}
```

## CLI Examples

```bash
# Simple render
fastrender page.html screenshot.png

# Full HD retina
fastrender page.html screenshot.png --width 1920 --height 1080 --dpi 2.0

# JPEG with quality
fastrender page.html photo.jpg --quality 85

# Custom fonts
fastrender page.html output.png --font-dir /path/to/fonts

# From stdin
curl https://example.com | fastrender - output.png

# Verbose mode
fastrender page.html output.png --verbose
```

---

# Library Usage Examples

## Example 1: Simple Screenshot

```rust
use fastrender::Renderer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let html = r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                body { font-family: Arial; padding: 20px; }
                h1 { color: #333; }
            </style>
        </head>
        <body>
            <h1>Hello, World!</h1>
            <p>This is a test.</p>
        </body>
        </html>
    "#;

    let renderer = Renderer::new();
    let image = renderer.render(html, 800, 600)?;
    image.save("screenshot.png")?;

    Ok(())
}
```

## Example 2: PDF Generation

```rust
use fastrender::{Renderer, RenderOptions, MediaType};

fn html_to_pdf(html: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    // A4 dimensions at 96 DPI
    const A4_WIDTH: u32 = 794;
    const A4_HEIGHT: u32 = 1123;

    let renderer = Renderer::builder()
        .viewport_size(A4_WIDTH, A4_HEIGHT)
        .default_font_size(12.0)
        .build()?;

    // Render as print media
    let options = RenderOptions {
        viewport_width: A4_WIDTH,
        viewport_height: A4_HEIGHT,
        media_type: MediaType::Print,
        ..Default::default()
    };

    let image = renderer.render(html, A4_WIDTH, A4_HEIGHT)?;

    // Convert to PDF (using external crate like printpdf)
    let pdf_bytes = convert_image_to_pdf(&image)?;

    Ok(pdf_bytes)
}

fn convert_image_to_pdf(image: &fastrender::Image) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    // Implementation using printpdf or similar
    todo!()
}
```

## Example 3: Thumbnail Service

```rust
use fastrender::{Renderer, OutputFormat};
use std::path::Path;

struct ThumbnailService {
    renderer: Renderer,
}

impl ThumbnailService {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let renderer = Renderer::builder()
            .viewport_size(800, 600)
            .build()?;

        Ok(Self { renderer })
    }

    fn generate_thumbnail(
        &self,
        html: &str,
        output_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Render at full size
        let image = self.renderer.render(html, 800, 600)?;

        // Resize to thumbnail
        let thumbnail = image.resize(200, 150);

        // Save as JPEG for smaller size
        thumbnail.save_as(output_path, OutputFormat::Jpeg { quality: 85 })?;

        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let service = ThumbnailService::new()?;

    let html = std::fs::read_to_string("page.html")?;
    service.generate_thumbnail(&html, Path::new("thumb.jpg"))?;

    Ok(())
}
```

## Example 4: Testing Tool

```rust
use fastrender::{Renderer, Color};

#[test]
fn test_red_box_renders() {
    let html = r#"
        <div style="width: 100px; height: 100px; background: red;"></div>
    "#;

    let renderer = Renderer::new();
    let image = renderer.render(html, 800, 600).unwrap();

    // Check pixel at (50, 50) is red
    let pixel = image.get_pixel(50, 50);
    assert_eq!(pixel, Color::rgb(255, 0, 0));
}

#[test]
fn test_flexbox_layout() {
    let html = r#"
        <div style="display: flex; gap: 10px;">
            <div style="flex: 1; background: blue;">Item 1</div>
            <div style="flex: 2; background: green;">Item 2</div>
        </div>
    "#;

    let renderer = Renderer::new();
    let image = renderer.render(html, 800, 600).unwrap();

    // Verify rendering succeeded
    assert_eq!(image.width(), 800);
    assert_eq!(image.height(), 600);

    // Could add pixel-level checks here
}
```

## Example 5: Custom Fonts

```rust
use fastrender::{Renderer, FontConfig};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let html = r#"
        <!DOCTYPE html>
        <html>
        <head>
            <style>
                @font-face {
                    font-family: 'CustomFont';
                    src: url('CustomFont.ttf');
                }
                body {
                    font-family: 'CustomFont', 'DejaVu Sans', Arial;
                }
            </style>
        </head>
        <body>
            <h1>Custom Font Test</h1>
        </body>
        </html>
    "#;

    let font_config = FontConfig::new()
        .add_font_file("fonts/CustomFont.ttf")
        .add_font_file("fonts/CustomFont-Bold.ttf")
        .add_fallback("DejaVu Sans")
        .add_fallback("Arial");

    let renderer = Renderer::builder()
        .font_config(font_config)
        .build()?;

    let image = renderer.render(html, 800, 600)?;
    image.save("custom-font.png")?;

    Ok(())
}
```

## Example 6: Batch Processing

```rust
use fastrender::Renderer;
use rayon::prelude::*;
use std::path::PathBuf;

fn batch_render(
    html_files: Vec<PathBuf>,
) -> Result<Vec<PathBuf>, Box<dyn std::error::Error>> {
    let renderer = Renderer::new();

    let results: Result<Vec<_>, _> = html_files
        .par_iter()
        .map(|html_path| {
            let html = std::fs::read_to_string(html_path)?;
            let image = renderer.render(&html, 800, 600)?;

            let output_path = html_path.with_extension("png");
            image.save(&output_path)?;

            Ok(output_path)
        })
        .collect();

    results
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let html_files = vec![
        PathBuf::from("page1.html"),
        PathBuf::from("page2.html"),
        PathBuf::from("page3.html"),
    ];

    let output_files = batch_render(html_files)?;
    println!("Rendered {} files", output_files.len());

    Ok(())
}
```

## Example 7: Web Server Integration (Axum)

```rust
use axum::{
    extract::Json,
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::post,
    Router,
};
use fastrender::Renderer;
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
struct RenderRequest {
    html: String,
    width: u32,
    height: u32,
}

#[derive(Serialize)]
struct RenderResponse {
    image_base64: String,
}

async fn render_handler(
    Json(request): Json<RenderRequest>,
) -> Result<Json<RenderResponse>, StatusCode> {
    let renderer = Renderer::new();

    let image = renderer
        .render(&request.html, request.width, request.height)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let png_bytes = image
        .encode_png()
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    let image_base64 = base64::encode(&png_bytes);

    Ok(Json(RenderResponse { image_base64 }))
}

#[tokio::main]
async fn main() {
    let app = Router::new().route("/render", post(render_handler));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .unwrap();

    axum::serve(listener, app).await.unwrap();
}
```

---

# API Stability Guarantees

## Semantic Versioning

FastRender follows **strict semver**:

```
MAJOR.MINOR.PATCH

1.0.0 → 1.0.1  Patch: Bug fixes only, no API changes
1.0.0 → 1.1.0  Minor: New features, backwards compatible
1.0.0 → 2.0.0  Major: Breaking changes
```

## Stability Levels

### Stable API

**Guaranteed stable (will not break):**
- `Renderer::new()`
- `Renderer::render()`
- `Image::save()`
- `Image::width()`, `Image::height()`
- `Color` struct
- `RenderError` variants

**Policy:** Breaking changes only in major versions.

### Unstable API

**May change in minor versions:**
- Async API (feature-gated)
- Internal types
- Debug representations

**Policy:** Breaking changes allowed in minor versions, but documented.

### Experimental API

**May change at any time:**
- Features behind `experimental` flag
- Explicitly marked in docs

**Policy:** No stability guarantees.

## Deprecation Policy

**Deprecating features:**

1. Add `#[deprecated]` attribute in version N
2. Keep deprecated feature for at least 2 major versions
3. Remove in version N+2

**Example:**

```rust
// Version 1.0
pub fn render_html(&self, html: &str) -> Result<Image>;

// Version 2.0 - deprecate
#[deprecated(since = "2.0.0", note = "use `render()` instead")]
pub fn render_html(&self, html: &str) -> Result<Image> {
    self.render(html, 800, 600)  // Call new API
}

// Version 4.0 - remove
// (function no longer exists)
```

## API Evolution Examples

### Adding New Parameter (Backwards Compatible)

```rust
// Version 1.0
pub fn render(&self, html: &str, width: u32, height: u32) -> Result<Image>;

// Version 1.1 - Add optional parameter via builder
impl Renderer {
    pub fn render_with_options(
        &self,
        html: &str,
        options: RenderOptions,
    ) -> Result<Image>;
}

// Old API still works!
renderer.render(html, 800, 600)?;

// New API available
let options = RenderOptions { width: 800, height: 600, ..Default::default() };
renderer.render_with_options(html, options)?;
```

### Extending Enum (Breaking Change)

```rust
// Version 1.0
pub enum OutputFormat {
    Png,
    Jpeg { quality: u8 },
}

// Version 2.0 - Add variant (breaking!)
pub enum OutputFormat {
    Png,
    Jpeg { quality: u8 },
    WebP { quality: u8, lossless: bool },  // New!
}

// This breaks exhaustive matching:
match format {
    OutputFormat::Png => { /* ... */ }
    OutputFormat::Jpeg { quality } => { /* ... */ }
    // ❌ Missing WebP case!
}
```

**Solution:** Use `#[non_exhaustive]` from start:

```rust
#[non_exhaustive]
pub enum OutputFormat {
    Png,
    Jpeg { quality: u8 },
}

// Forces users to add _ => { } catch-all
match format {
    OutputFormat::Png => { /* ... */ }
    OutputFormat::Jpeg { quality } => { /* ... */ }
    _ => { /* ... */ }  // Required due to #[non_exhaustive]
}

// Now adding WebP is non-breaking
```

---

# Internal vs External APIs

## Public API (External)

**What users interact with:**
- `Renderer`, `RendererBuilder`
- `Image`, `Color`, `FontConfig`
- `RenderError`
- Top-level functions

**Published in crate root:**
```rust
// src/lib.rs
pub use renderer::Renderer;
pub use image::Image;
// etc.
```

## Internal API (Private)

**Implementation details:**
- Layout algorithms
- CSS parser internals
- Font cache
- Display list
- Fragment tree

**Not re-exported:**
```rust
// src/layout/mod.rs
// (not pub use in lib.rs)
pub struct BlockLayout { }  // Only visible within crate
```

## Semi-Public API

**Advanced users only:**
- Behind feature flags
- Explicitly unstable
- For framework builders

**Example:**
```rust
// Only available with "internals" feature
#[cfg(feature = "internals")]
pub mod layout {
    pub use crate::layout_internal::*;
}
```

---

# Documentation Requirements

## Rustdoc Standards

**Every public item must have:**

1. **Summary line** - One line describing what it does
2. **Description** - Paragraph explaining purpose
3. **Examples** - At least one working example
4. **Panics section** - If it can panic
5. **Errors section** - If it returns Result
6. **Safety section** - If it's unsafe

**Example:**

```rust
/// Render HTML to an image
///
/// Takes an HTML string and renders it to a raster image at the specified
/// dimensions. The HTML is parsed, styled, laid out, and rasterized to
/// produce the output image.
///
/// # Arguments
///
/// * `html` - HTML string to render
/// * `width` - Viewport width in pixels
/// * `height` - Viewport height in pixels
///
/// # Returns
///
/// Returns an `Image` containing the rendered output, or a `RenderError`
/// if rendering fails.
///
/// # Errors
///
/// This function returns an error if:
/// - HTML parsing fails
/// - CSS parsing fails
/// - Layout fails (e.g., circular dependencies)
/// - Font loading fails
/// - Rasterization fails
///
/// # Examples
///
/// ```
/// use fastrender::Renderer;
///
/// let html = "<h1>Hello, World!</h1>";
/// let renderer = Renderer::new();
/// let image = renderer.render(html, 800, 600)?;
/// image.save("output.png")?;
/// # Ok::<(), fastrender::RenderError>(())
/// ```
pub fn render(
    &self,
    html: &str,
    width: u32,
    height: u32,
) -> Result<Image, RenderError> {
    // Implementation
}
```

## Example Requirements

**All examples must:**
- Compile without errors
- Be tested with `cargo test --doc`
- Show realistic usage
- Handle errors appropriately

## Module-Level Documentation

**Every module must have:**

```rust
//! Module summary
//!
//! Detailed description of what this module provides.
//!
//! # Examples
//!
//! ```
//! use fastrender::image::Image;
//! // Example usage
//! ```

pub mod image;
```

---

# Acceptance Criteria

- [ ] All public types have rustdoc with examples
- [ ] API follows Rust naming conventions
- [ ] Builder pattern used for configuration
- [ ] Errors are typed and descriptive
- [ ] Simple use cases require minimal code
- [ ] Complex use cases are possible
- [ ] CLI tool is functional and documented
- [ ] Async API is available (feature-gated)
- [ ] API stability guarantees documented
- [ ] Semver policy defined
- [ ] Examples compile and run
- [ ] Documentation is comprehensive
- [ ] Internal APIs are clearly separated
- [ ] Performance is acceptable (no API overhead)

## Timeline Estimates

| Phase | Duration | Effort | Dependencies |
|-------|----------|--------|--------------|
| API design | 1 week | 1 engineer | Architecture decisions |
| Core types implementation | 2 weeks | 1-2 engineers | Layout/rendering |
| Builder pattern | 1 week | 1 engineer | Core types |
| Error handling | 1 week | 1 engineer | Core types |
| CLI tool | 1 week | 1 engineer | Core API |
| Documentation | 2 weeks | 1 engineer + tech writer | All above |
| Examples | 1 week | 1 engineer | Documentation |
| API review | 1 week | Team | All above |

**Total:** ~10 weeks for complete API design and documentation

## Next Steps

1. **Review this document** with team
2. **Prototype core types** (Renderer, Image, etc.)
3. **Test API ergonomics** with examples
4. **Iterate on design** based on feedback
5. **Implement** following this spec
6. **Document** as you go
7. **Review** before 1.0 release

## References

- **Rust API Guidelines:** https://rust-lang.github.io/api-guidelines/
- **Semver:** https://semver.org/
- **Rustdoc Guide:** https://doc.rust-lang.org/rustdoc/
- **Builder Pattern:** https://rust-unofficial.github.io/patterns/patterns/creational/builder.html
- **Error Handling:** https://doc.rust-lang.org/book/ch09-00-error-handling.html

---

**Last Updated:** 2025-11-19
**Status:** Living Document
**Maintained By:** FastRender API Team
**Review Frequency:** Before each major/minor release
**Version:** 1.0
