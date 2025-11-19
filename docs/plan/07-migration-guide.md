# Phase 7: Migration Guide - V1 to V2

**Duration:** Reference document (ongoing through V2 development)
**Prerequisites:**
- V2 implementation in progress
- Clear understanding of V1 architecture
- Feature parity tracking
**Dependencies:**
- V2 implementation progress
- API design (11-api-design.md)
- Feature roadmap (05-css-features-roadmap.md)
**Output:** Complete migration guide for users transitioning from V1 to V2

## Objectives

This guide provides everything users need to migrate from FastRender V1 to V2:

- **Timeline and deprecation schedule** - When V1 will be deprecated and removed
- **Feature flags for coexistence** - Run V1 and V2 side-by-side during migration
- **Breaking API changes** - What changed and why
- **Migration checklist** - Step-by-step upgrade process
- **Backwards compatibility** - What still works, what needs changes
- **Code migration examples** - Before/after code for common patterns
- **Testing strategy** - How to verify migration success
- **Performance comparison** - V1 vs V2 benchmarks
- **Feature parity milestones** - When to switch from V1 to V2
- **Rollback procedures** - How to revert if V2 has issues

This guide is **critical for adoption** - users need confidence they can migrate safely.

## Context

### Why V2?

FastRender V1 has fundamental architectural limitations:

1. **Table Layout Broken**: Tables forced into flexbox, causing incorrect rendering
2. **Text Layout Incomplete**: No proper line breaking, BiDi, or shaping
3. **Mixed Concerns**: Style, layout, and rendering tightly coupled
4. **Limited CSS Support**: Missing critical features (margin collapse, pseudo-elements, media queries)
5. **Hard to Test**: No separation between layout and rendering
6. **Performance Issues**: No incremental layout, excessive re-computation

V2 addresses all these issues with a **complete architectural rebuild** based on browser rendering principles.

### V1 Architecture (What We're Moving From)

```rust
// V1 simplified architecture
HTML → DOM → StyledNode → Layout (Taffy for everything) → Render

Problems:
- StyledNode conflates style and layout
- Taffy forced to handle tables (doesn't work)
- No proper text pipeline
- Direct rendering from layout
```

### V2 Architecture (What We're Moving To)

```rust
// V2 architecture
HTML → DOM → Box Tree → Fragment Tree → Display List → Pixels

Improvements:
- Separate box tree (styled) from fragment tree (layout)
- Independent layout algorithms per formatting context
- Proper text pipeline (bidi, shaping, line breaking)
- Display list abstraction
- Clear separation of concerns
```

### The Problem V1 Has

V1 users face these issues:

1. **Tables render incorrectly** - Colspan, rowspan, auto-width don't work
2. **Text rendering is basic** - No line breaking, BiDi, or complex scripts
3. **Performance degrades** - Large pages re-layout completely on any change
4. **Limited CSS** - Missing margin collapse, pseudo-elements, many properties
5. **Hard to debug** - No visibility into layout decisions
6. **API limitations** - Cannot configure viewport, DPI, or fonts properly

## Migration Timeline

### Phase 1: V2 Development (Months 1-6)

**Status:** V2 is being built

- V1 remains stable and supported
- V2 developed in parallel
- No API changes to V1
- Users can test V2 via feature flag

### Phase 2: V2 Beta (Months 7-9)

**Status:** V2 reaches feature parity

- V2 marked as "beta"
- Documentation for V2 API
- Migration guide published (this document)
- V1 still default, V2 opt-in
- Deprecation warnings added to V1

### Phase 3: V2 Stable (Months 10-12)

**Status:** V2 becomes default

- V2 marked as "stable"
- V2 becomes default renderer
- V1 available via legacy flag
- V1 enters maintenance mode (security fixes only)
- Migration tooling provided

### Phase 4: V1 Deprecation (Months 13-18)

**Status:** V1 deprecated

- V1 marked as deprecated in all documentation
- Warnings when using V1 API
- V2 is only actively developed version
- V1 still available for backwards compatibility
- Final migration deadline announced (6 months out)

### Phase 5: V1 Removal (Month 19+)

**Status:** V1 code removed

- V1 code removed from codebase
- Only V2 API exists
- Major version bump (2.0)
- No backwards compatibility with V1

### Current Status

As of **2025-11-19**:
- **Phase:** Phase 1 (V2 Development)
- **V2 Completion:** ~40% (foundation complete, layout in progress)
- **Feature Parity:** ~30%
- **Migration Readiness:** Not yet (V2 not feature-complete)
- **Recommended Action:** Stay on V1, monitor V2 progress

## Feature Flags for V1/V2 Coexistence

During migration, users can run both V1 and V2 side-by-side to test compatibility.

### Cargo Feature Flags

**File: `Cargo.toml`**

```toml
[features]
# Default: V2 (once stable)
default = ["v2"]

# V1 legacy renderer
v1 = []

# V2 new renderer
v2 = []

# Both (for migration testing)
dual = ["v1", "v2"]
```

### Dependency Configuration

```bash
# V2 only (default, recommended)
cargo add fastrender

# V1 only (legacy)
cargo add fastrender --no-default-features --features v1

# Both V1 and V2 (for migration testing)
cargo add fastrender --no-default-features --features dual
```

### Runtime Selection

When both are available, select at runtime:

```rust
use fastrender::{Renderer, RenderBackend};

fn main() {
    // V2 (default)
    let renderer = Renderer::new();

    // V1 (explicit)
    let renderer_v1 = Renderer::new_v1();

    // Select based on environment variable
    let renderer = if std::env::var("FASTRENDER_V1").is_ok() {
        Renderer::new_v1()
    } else {
        Renderer::new()  // V2
    };

    // Render with selected backend
    let html = "<html><body>Hello</body></html>";
    let image = renderer.render(html, 800, 600)?;
}
```

### Testing Both Versions

Compare V1 and V2 output side-by-side:

```rust
#[cfg(feature = "dual")]
fn test_migration(html: &str) {
    let v1 = fastrender::Renderer::new_v1();
    let v2 = fastrender::Renderer::new();

    let img1 = v1.render(html, 800, 600).unwrap();
    let img2 = v2.render(html, 800, 600).unwrap();

    // Save both for visual comparison
    img1.save("output-v1.png");
    img2.save("output-v2.png");

    // Compare pixels
    let diff = compare_images(&img1, &img2);
    println!("Difference: {:.2}%", diff.mismatch_percentage() * 100.0);
}
```

## Breaking API Changes

### Change 1: Renderer Constructor

**V1:**
```rust
use fastrender::Renderer;

let renderer = Renderer::new();
```

**V2:**
```rust
use fastrender::{Renderer, RenderOptions};

// Default options
let renderer = Renderer::new();

// Or with custom options
let options = RenderOptions {
    viewport_width: 1920,
    viewport_height: 1080,
    device_pixel_ratio: 2.0,
    default_font_size: 16.0,
    ..Default::default()
};
let renderer = Renderer::with_options(options);
```

**Why:** V2 needs more configuration options for proper rendering.

**Migration:**
- Simple cases: No change needed (`Renderer::new()` still works)
- Custom viewports: Use `RenderOptions`

### Change 2: Render Method Signature

**V1:**
```rust
let image = renderer.render_html(html_str)?;
// Returns Pixmap with default size
```

**V2:**
```rust
let image = renderer.render(html_str, 800, 600)?;
// Explicit width and height required
```

**Why:** V2 needs explicit viewport dimensions for proper layout.

**Migration:**
```rust
// V1 code
let image = renderer.render_html(html_str)?;

// V2 migration
let image = renderer.render(html_str, 800, 600)?;
//                                    ^^^^^^^^ Add dimensions
```

### Change 3: Font Configuration

**V1:**
```rust
// V1 uses system fonts automatically
let renderer = Renderer::new();
```

**V2:**
```rust
use fastrender::{Renderer, FontConfig};

// Explicit font configuration
let font_config = FontConfig::new()
    .add_font_dir("/usr/share/fonts")
    .add_fallback("DejaVu Sans")
    .add_fallback("Noto Sans");

let renderer = Renderer::builder()
    .font_config(font_config)
    .build();
```

**Why:** V2 gives explicit control over font loading and fallback.

**Migration:**
```rust
// V1 code (implicit fonts)
let renderer = Renderer::new();

// V2 migration (explicit fonts)
let renderer = Renderer::builder()
    .font_config(FontConfig::system_fonts())  // Use system fonts
    .build();
```

### Change 4: Error Types

**V1:**
```rust
pub enum RenderError {
    ParseError(String),
    LayoutError(String),
}
```

**V2:**
```rust
pub enum RenderError {
    HtmlParse(html5ever::ParseError),
    CssParse(cssparser::ParseError),
    Layout(LayoutError),
    Font(FontError),
    Raster(RasterError),
}
```

**Why:** V2 has more granular error types for better debugging.

**Migration:**
```rust
// V1 code
match result {
    Err(RenderError::ParseError(msg)) => {
        eprintln!("Parse failed: {}", msg);
    }
    _ => {}
}

// V2 migration
match result {
    Err(RenderError::HtmlParse(e)) | Err(RenderError::CssParse(e)) => {
        eprintln!("Parse failed: {}", e);
    }
    _ => {}
}
```

### Change 5: Output Format Options

**V1:**
```rust
// Only PNG output
let pixmap = renderer.render_html(html)?;
pixmap.save_png("output.png")?;
```

**V2:**
```rust
use fastrender::OutputFormat;

// PNG (default)
renderer.render_to_file(html, 800, 600, "output.png")?;

// JPEG
renderer.render_to_file(html, 800, 600, "output.jpg")?;

// Or explicit format
let image = renderer.render(html, 800, 600)?;
image.save_as("output.jpg", OutputFormat::Jpeg { quality: 90 })?;
```

**Why:** V2 supports multiple output formats.

**Migration:**
```rust
// V1 code
let pixmap = renderer.render_html(html)?;
pixmap.save_png("output.png")?;

// V2 migration
renderer.render_to_file(html, 800, 600, "output.png")?;
```

### Change 6: CSS Variable Support

**V1:**
```css
/* CSS variables parsed but not applied correctly */
:root {
    --main-color: blue;
}
div {
    color: var(--main-color);  /* May not work */
}
```

**V2:**
```css
/* CSS variables work correctly with inheritance and fallbacks */
:root {
    --main-color: blue;
    --spacing: 10px;
}
div {
    color: var(--main-color);           /* Works */
    padding: var(--spacing, 5px);       /* Fallback works */
    margin: var(--undefined, 0);        /* Fallback used */
}
```

**Migration:** No code changes needed, but verify CSS variable usage works correctly.

### Change 7: Table Layout

**V1:**
```html
<!-- Tables may render incorrectly -->
<table>
    <tr>
        <td colspan="2">Merged</td>  <!-- Colspan broken in V1 -->
    </tr>
</table>
```

**V2:**
```html
<!-- Tables render correctly -->
<table>
    <tr>
        <td colspan="2">Merged</td>  <!-- Colspan works -->
        <td rowspan="2">Tall</td>    <!-- Rowspan works -->
    </tr>
</table>
```

**Migration:** Test all table layouts, verify correctness in V2.

## Migration Checklist

### Pre-Migration Assessment

- [ ] **Audit your HTML/CSS usage**
  - Identify table layouts (major V1 bug)
  - Identify text-heavy pages (V2 has better text)
  - Identify CSS variable usage (V2 fixes this)
  - List all CSS properties used

- [ ] **Review dependencies**
  - Check if other crates depend on V1 API
  - Identify version pinning in Cargo.toml
  - Check if V1-specific workarounds exist

- [ ] **Set up test environment**
  - Enable dual feature flag
  - Create test suite comparing V1 vs V2 output
  - Set up visual regression tests

### Migration Steps

#### Step 1: Enable Dual Mode (Week 1)

```toml
# Cargo.toml
[dependencies]
fastrender = { version = "2.0", features = ["dual"] }
```

```rust
// Test both renderers
#[cfg(test)]
mod migration_tests {
    use fastrender::{Renderer, RenderOptions};

    #[test]
    fn compare_renderers() {
        let html = include_str!("../test_pages/homepage.html");

        let v1_renderer = Renderer::new_v1();
        let v2_renderer = Renderer::new();

        let v1_output = v1_renderer.render(html, 800, 600).unwrap();
        let v2_output = v2_renderer.render(html, 800, 600).unwrap();

        v1_output.save("migration/v1.png").unwrap();
        v2_output.save("migration/v2.png").unwrap();

        // Visual inspection needed
    }
}
```

#### Step 2: Update API Calls (Week 1-2)

```rust
// Before (V1)
let renderer = Renderer::new();
let image = renderer.render_html(html_string)?;

// After (V2)
let renderer = Renderer::new();
let image = renderer.render(html_string, 800, 600)?;
```

#### Step 3: Add Font Configuration (Week 2)

```rust
// V2 with system fonts
let renderer = Renderer::builder()
    .font_config(FontConfig::system_fonts())
    .build();

// Or custom fonts
let font_config = FontConfig::new()
    .add_font_dir("/path/to/fonts")
    .add_font_file("CustomFont.ttf")
    .add_fallback("Arial");

let renderer = Renderer::builder()
    .font_config(font_config)
    .build();
```

#### Step 4: Update Error Handling (Week 2)

```rust
// Before (V1)
match renderer.render_html(html) {
    Ok(image) => { /* ... */ }
    Err(RenderError::ParseError(msg)) => {
        eprintln!("Parse error: {}", msg);
    }
    Err(RenderError::LayoutError(msg)) => {
        eprintln!("Layout error: {}", msg);
    }
}

// After (V2)
match renderer.render(html, 800, 600) {
    Ok(image) => { /* ... */ }
    Err(RenderError::HtmlParse(e)) => {
        eprintln!("HTML parse error: {}", e);
    }
    Err(RenderError::CssParse(e)) => {
        eprintln!("CSS parse error: {}", e);
    }
    Err(RenderError::Layout(e)) => {
        eprintln!("Layout error: {:?}", e);
    }
    Err(RenderError::Font(e)) => {
        eprintln!("Font error: {:?}", e);
    }
    Err(RenderError::Raster(e)) => {
        eprintln!("Raster error: {:?}", e);
    }
}
```

#### Step 5: Test All Features (Week 3)

Create comprehensive tests:

```rust
#[cfg(test)]
mod v2_migration_tests {
    use super::*;

    #[test]
    fn test_basic_layout() {
        let html = r#"
            <!DOCTYPE html>
            <html>
            <head>
                <style>
                    body { margin: 0; padding: 20px; }
                    .box { width: 100px; height: 100px; background: blue; }
                </style>
            </head>
            <body>
                <div class="box"></div>
            </body>
            </html>
        "#;

        let renderer = Renderer::new();
        let image = renderer.render(html, 800, 600).unwrap();

        // Verify rendering succeeded
        assert_eq!(image.width(), 800);
        assert_eq!(image.height(), 600);
    }

    #[test]
    fn test_table_layout() {
        let html = r#"
            <table>
                <tr>
                    <td colspan="2">Wide cell</td>
                </tr>
                <tr>
                    <td>Cell 1</td>
                    <td>Cell 2</td>
                </tr>
            </table>
        "#;

        let renderer = Renderer::new();
        let image = renderer.render(html, 800, 600).unwrap();

        // Table should render (V1 would fail)
        assert!(image.width() > 0);
    }

    #[test]
    fn test_css_variables() {
        let html = r#"
            <style>
                :root { --color: red; }
                div { color: var(--color); }
            </style>
            <div>Red text</div>
        "#;

        let renderer = Renderer::new();
        let image = renderer.render(html, 800, 600).unwrap();

        // Should render without error
        assert!(image.width() > 0);
    }

    #[test]
    fn test_text_rendering() {
        let html = r#"
            <div style="width: 200px;">
                This is a long line of text that should wrap correctly
                across multiple lines based on the width.
            </div>
        "#;

        let renderer = Renderer::new();
        let image = renderer.render(html, 800, 600).unwrap();

        // Should render without error
        assert!(image.width() > 0);
    }
}
```

#### Step 6: Performance Testing (Week 3-4)

```rust
use std::time::Instant;

fn benchmark_renderer(html: &str, iterations: usize) {
    let renderer = Renderer::new();

    let start = Instant::now();
    for _ in 0..iterations {
        let _ = renderer.render(html, 800, 600).unwrap();
    }
    let duration = start.elapsed();

    println!("Rendered {} times in {:?}", iterations, duration);
    println!("Average: {:?} per render", duration / iterations as u32);
}

#[test]
fn compare_performance() {
    let html = include_str!("../test_pages/complex.html");

    println!("V1 Performance:");
    let v1 = Renderer::new_v1();
    benchmark_with_renderer(&v1, html, 100);

    println!("\nV2 Performance:");
    let v2 = Renderer::new();
    benchmark_with_renderer(&v2, html, 100);
}
```

#### Step 7: Switch to V2 Only (Week 4)

```toml
# Cargo.toml
[dependencies]
fastrender = "2.0"  # V2 only, remove dual flag
```

Remove V1-specific code:

```rust
// Remove all Renderer::new_v1() calls
// All code now uses Renderer::new() (V2)
```

#### Step 8: Production Rollout (Week 5+)

- Deploy to staging environment
- Monitor for errors and regressions
- Gradually roll out to production (10% → 50% → 100%)
- Keep rollback plan ready

### Post-Migration Verification

- [ ] All tests pass with V2
- [ ] Visual regression tests pass
- [ ] Performance meets requirements
- [ ] No V1 code remains
- [ ] Documentation updated
- [ ] Team trained on V2 API

## Backwards Compatibility

### What Still Works

These APIs are **unchanged** between V1 and V2:

```rust
// Basic rendering
let renderer = Renderer::new();
let image = renderer.render(html, width, height)?;

// Error handling (Result type)
match renderer.render(html, 800, 600) {
    Ok(img) => { /* use image */ }
    Err(e) => { /* handle error */ }
}

// Image output
image.save("output.png")?;
```

### What Changed

These require **code updates**:

1. **Explicit dimensions**: `render()` now requires width and height
2. **Font configuration**: More control, requires explicit setup
3. **Error types**: More granular error variants
4. **Options**: More configuration options available

### Compatibility Layer

For gradual migration, use the compatibility layer:

```rust
// V1-style API (compatibility layer)
mod v1_compat {
    use fastrender::{Renderer, RenderError};

    pub fn render_html(html: &str) -> Result<Image, RenderError> {
        let renderer = Renderer::new();
        renderer.render(html, 800, 600)  // Default dimensions
    }
}

// Use compatibility layer
let image = v1_compat::render_html(html_string)?;
```

## Code Migration Examples

### Example 1: Simple Web Page Screenshot

**Before (V1):**
```rust
use fastrender::Renderer;

fn screenshot_page(html: &str) -> Result<(), Box<dyn std::error::Error>> {
    let renderer = Renderer::new();
    let image = renderer.render_html(html)?;
    image.save_png("screenshot.png")?;
    Ok(())
}
```

**After (V2):**
```rust
use fastrender::Renderer;

fn screenshot_page(html: &str) -> Result<(), Box<dyn std::error::Error>> {
    let renderer = Renderer::new();
    let image = renderer.render(html, 1920, 1080)?;
    image.save("screenshot.png")?;
    Ok(())
}
```

**Changes:**
- Added explicit dimensions (1920, 1080)
- `render_html()` → `render()`
- `save_png()` → `save()` (format inferred from extension)

### Example 2: PDF Generation

**Before (V1):**
```rust
use fastrender::Renderer;

fn html_to_pdf(html: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let renderer = Renderer::new();
    let image = renderer.render_html(html)?;

    // Convert to PDF (using external crate)
    let pdf_bytes = image_to_pdf(&image)?;
    Ok(pdf_bytes)
}
```

**After (V2):**
```rust
use fastrender::{Renderer, RenderOptions};

fn html_to_pdf(html: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    // Configure for print media
    let options = RenderOptions {
        viewport_width: 794,   // A4 width in pixels at 96 DPI
        viewport_height: 1123, // A4 height in pixels at 96 DPI
        device_pixel_ratio: 1.0,
        default_font_size: 12.0,
        ..Default::default()
    };

    let renderer = Renderer::with_options(options);
    let image = renderer.render(html, 794, 1123)?;

    // Convert to PDF
    let pdf_bytes = image_to_pdf(&image)?;
    Ok(pdf_bytes)
}
```

**Changes:**
- Added `RenderOptions` for print configuration
- Explicit A4 dimensions
- Font size control

### Example 3: Thumbnail Service

**Before (V1):**
```rust
use fastrender::Renderer;

struct ThumbnailService {
    renderer: Renderer,
}

impl ThumbnailService {
    fn new() -> Self {
        Self {
            renderer: Renderer::new(),
        }
    }

    fn generate_thumbnail(&self, html: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        let image = self.renderer.render_html(html)?;

        // Resize to thumbnail
        let thumbnail = image.resize(200, 150);
        Ok(thumbnail.encode_png())
    }
}
```

**After (V2):**
```rust
use fastrender::{Renderer, RenderOptions, OutputFormat};

struct ThumbnailService {
    renderer: Renderer,
}

impl ThumbnailService {
    fn new() -> Self {
        let options = RenderOptions {
            viewport_width: 800,
            viewport_height: 600,
            device_pixel_ratio: 1.0,
            ..Default::default()
        };

        Self {
            renderer: Renderer::with_options(options),
        }
    }

    fn generate_thumbnail(&self, html: &str) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        // Render at full size
        let image = self.renderer.render(html, 800, 600)?;

        // Resize to thumbnail
        let thumbnail = image.resize(200, 150);

        // Encode as JPEG for smaller file size
        Ok(thumbnail.encode(OutputFormat::Jpeg { quality: 85 })?)
    }
}
```

**Changes:**
- Explicit viewport configuration
- Explicit dimensions in render()
- JPEG output option for smaller thumbnails

### Example 4: Testing Tool

**Before (V1):**
```rust
use fastrender::Renderer;

#[test]
fn test_layout() {
    let html = r#"
        <div style="width: 100px; height: 100px; background: red;"></div>
    "#;

    let renderer = Renderer::new();
    let image = renderer.render_html(html).unwrap();

    // Check pixel at (50, 50) is red
    let pixel = image.get_pixel(50, 50);
    assert_eq!(pixel.red(), 255);
}
```

**After (V2):**
```rust
use fastrender::Renderer;

#[test]
fn test_layout() {
    let html = r#"
        <div style="width: 100px; height: 100px; background: red;"></div>
    "#;

    let renderer = Renderer::new();
    let image = renderer.render(html, 800, 600).unwrap();

    // Check pixel at (50, 50) is red
    let pixel = image.get_pixel(50, 50);
    assert_eq!(pixel.red(), 255);
}
```

**Changes:**
- Explicit dimensions (800, 600)

### Example 5: Custom Font Handling

**Before (V1):**
```rust
use fastrender::Renderer;

// V1 uses system fonts automatically
fn render_with_custom_font(html: &str) -> Result<Image, Box<dyn std::error::Error>> {
    let renderer = Renderer::new();
    let image = renderer.render_html(html)?;
    Ok(image)
}
```

**After (V2):**
```rust
use fastrender::{Renderer, FontConfig};

fn render_with_custom_font(html: &str) -> Result<Image, Box<dyn std::error::Error>> {
    // Configure custom fonts
    let font_config = FontConfig::new()
        .add_font_file("fonts/CustomFont.ttf")
        .add_font_file("fonts/CustomFont-Bold.ttf")
        .add_fallback("DejaVu Sans")
        .add_fallback("Arial");

    let renderer = Renderer::builder()
        .font_config(font_config)
        .build();

    let image = renderer.render(html, 800, 600)?;
    Ok(image)
}
```

**Changes:**
- Explicit font configuration
- Font fallback chain
- Builder pattern for options

## Testing Both Versions During Migration

### Visual Regression Testing

```rust
use fastrender::Renderer;
use image_compare::Algorithm;

fn test_v1_v2_compatibility(html: &str) {
    let v1 = Renderer::new_v1();
    let v2 = Renderer::new();

    let img1 = v1.render(html, 800, 600).unwrap();
    let img2 = v2.render(html, 800, 600).unwrap();

    // Save for manual inspection
    img1.save("test/v1.png").unwrap();
    img2.save("test/v2.png").unwrap();

    // Automated comparison
    let diff = image_compare::compare(&img1, &img2, Algorithm::RootMeanSquared);

    if diff.score < 0.95 {
        // Significant difference - investigate
        println!("WARNING: V1 and V2 differ significantly");
        println!("Similarity score: {:.2}%", diff.score * 100.0);

        // Save diff image
        diff.image.save("test/diff.png").unwrap();
    } else {
        println!("V1 and V2 output is similar: {:.2}%", diff.score * 100.0);
    }
}
```

### Automated Test Suite

```rust
#[cfg(test)]
mod migration_tests {
    use super::*;

    macro_rules! test_both_versions {
        ($name:ident, $html:expr) => {
            #[test]
            fn $name() {
                let html = $html;

                // V1 should not panic
                let v1 = Renderer::new_v1();
                let result_v1 = v1.render(html, 800, 600);
                assert!(result_v1.is_ok(), "V1 failed");

                // V2 should not panic
                let v2 = Renderer::new();
                let result_v2 = v2.render(html, 800, 600);
                assert!(result_v2.is_ok(), "V2 failed");

                // Compare outputs
                test_v1_v2_compatibility(html);
            }
        };
    }

    test_both_versions!(test_basic_div, r#"
        <div style="width: 100px; height: 100px; background: blue;"></div>
    "#);

    test_both_versions!(test_flexbox, r#"
        <div style="display: flex; gap: 10px;">
            <div style="flex: 1; background: red;">Item 1</div>
            <div style="flex: 2; background: blue;">Item 2</div>
        </div>
    "#);

    test_both_versions!(test_grid, r#"
        <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 10px;">
            <div>Cell 1</div>
            <div>Cell 2</div>
        </div>
    "#);

    test_both_versions!(test_table, r#"
        <table>
            <tr>
                <td colspan="2">Wide</td>
            </tr>
            <tr>
                <td>Cell 1</td>
                <td>Cell 2</td>
            </tr>
        </table>
    "#);
}
```

## Performance Comparison: V1 vs V2

### Benchmark Setup

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fastrender::Renderer;

fn benchmark_simple_page(c: &mut Criterion) {
    let html = include_str!("../test_pages/simple.html");

    let mut group = c.benchmark_group("simple_page");

    // V1 benchmark
    let v1_renderer = Renderer::new_v1();
    group.bench_function("v1", |b| {
        b.iter(|| {
            v1_renderer.render(black_box(html), 800, 600).unwrap()
        })
    });

    // V2 benchmark
    let v2_renderer = Renderer::new();
    group.bench_function("v2", |b| {
        b.iter(|| {
            v2_renderer.render(black_box(html), 800, 600).unwrap()
        })
    });

    group.finish();
}

fn benchmark_table_layout(c: &mut Criterion) {
    let html = r#"
        <table>
            <tr><td>Cell 1</td><td>Cell 2</td></tr>
            <tr><td colspan="2">Wide cell</td></tr>
        </table>
    "#;

    let mut group = c.benchmark_group("table_layout");

    // V1 will be slow/incorrect
    let v1_renderer = Renderer::new_v1();
    group.bench_function("v1", |b| {
        b.iter(|| {
            v1_renderer.render(black_box(html), 800, 600).unwrap()
        })
    });

    // V2 should be correct and possibly faster
    let v2_renderer = Renderer::new();
    group.bench_function("v2", |b| {
        b.iter(|| {
            v2_renderer.render(black_box(html), 800, 600).unwrap()
        })
    });

    group.finish();
}

criterion_group!(benches, benchmark_simple_page, benchmark_table_layout);
criterion_main!(benches);
```

### Expected Performance Characteristics

| Scenario | V1 Performance | V2 Performance | Notes |
|----------|----------------|----------------|-------|
| Simple block layout | Baseline (1x) | ~1x | Similar performance |
| Flexbox layout | Fast (Taffy) | Fast (Taffy) | Same engine |
| Grid layout | Fast (Taffy) | Fast (Taffy) | Same engine |
| Table layout | Slow/Broken | Correct | V2 fixes correctness |
| Complex text | Basic | Slower but correct | V2 does proper shaping |
| Large pages | No caching | Incremental layout | V2 can be faster |
| CSS variables | Buggy | Correct | V2 fixes correctness |

### Performance Regression Detection

```rust
// CI performance test
#[test]
fn performance_regression_check() {
    let html = include_str!("../test_pages/typical.html");

    let renderer = Renderer::new();

    let start = std::time::Instant::now();
    for _ in 0..100 {
        let _ = renderer.render(html, 800, 600).unwrap();
    }
    let duration = start.elapsed();

    let avg_ms = duration.as_millis() / 100;

    // Fail if slower than threshold
    assert!(avg_ms < 50, "Performance regression: avg {}ms > 50ms", avg_ms);
}
```

## When to Switch: Feature Parity Milestones

### Milestone 1: Core Layout (Month 3)

**Features:**
- ✅ Block layout with margin collapse
- ✅ Inline layout with line breaking
- ✅ Flexbox (via Taffy)
- ✅ Grid (via Taffy)
- ✅ Basic positioning

**Recommendation:** **Not ready** - Text rendering incomplete

### Milestone 2: Text Complete (Month 5)

**Features:**
- ✅ Text shaping (HarfBuzz/rustybuzz)
- ✅ Line breaking (UAX #14)
- ✅ BiDi support (UAX #9)
- ✅ Font fallback
- ✅ Basic CSS text properties

**Recommendation:** **Ready for testing** - Try V2 for text-heavy content

### Milestone 3: Table Layout (Month 6)

**Features:**
- ✅ Table layout algorithm
- ✅ Colspan/rowspan
- ✅ Auto width calculation
- ✅ Border collapse

**Recommendation:** **Switch if using tables** - V1 tables are broken

### Milestone 4: Visual Polish (Month 8)

**Features:**
- ✅ Border radius
- ✅ Box shadows
- ✅ Background images
- ✅ Pseudo-elements (::before, ::after)

**Recommendation:** **Ready for production** - Feature parity with V1

### Milestone 5: Advanced Features (Month 10)

**Features:**
- ✅ Media queries
- ✅ CSS variables (correct)
- ✅ Transforms
- ✅ Filters

**Recommendation:** **V2 becomes default** - Superset of V1 features

### Current Milestone (2025-11-19)

**Status:** Between Milestone 1 and 2

**Completed:**
- ✅ Foundation architecture
- ✅ Box tree generation
- ✅ Basic layout algorithms
- 🚧 Text shaping pipeline (in progress)
- ❌ Table layout (not started)
- ❌ Visual effects (not started)

**Recommendation:** **Stay on V1** - V2 not ready for production

## Rollback Plan

If V2 has issues, here's how to rollback:

### Step 1: Keep V1 Available

```toml
# Always maintain V1 compatibility during migration
[dependencies]
fastrender = { version = "2.0", features = ["dual"] }
```

### Step 2: Environment Variable Fallback

```rust
use std::env;
use fastrender::Renderer;

fn create_renderer() -> Renderer {
    if env::var("FASTRENDER_USE_V1").is_ok() {
        println!("WARNING: Using V1 fallback");
        Renderer::new_v1()
    } else {
        Renderer::new()  // V2
    }
}
```

### Step 3: Feature Flag Rollback

```rust
// Compile-time selection
#[cfg(feature = "force_v1")]
fn create_renderer() -> Renderer {
    Renderer::new_v1()
}

#[cfg(not(feature = "force_v1"))]
fn create_renderer() -> Renderer {
    Renderer::new()
}
```

### Step 4: Gradual Rollout with Rollback

```rust
use rand::Rng;

fn create_renderer_with_rollout(rollout_percentage: u32) -> Renderer {
    let mut rng = rand::thread_rng();
    let random: u32 = rng.gen_range(0..100);

    if random < rollout_percentage {
        // Use V2
        Renderer::new()
    } else {
        // Use V1
        Renderer::new_v1()
    }
}

// Usage:
// 10% V2, 90% V1
let renderer = create_renderer_with_rollout(10);

// If V2 has issues, drop to 0%
let renderer = create_renderer_with_rollout(0);  // All V1
```

### Step 5: Monitoring and Alerts

```rust
use std::time::Instant;

fn render_with_monitoring(html: &str) -> Result<Image, RenderError> {
    let renderer = Renderer::new();  // V2

    let start = Instant::now();
    let result = renderer.render(html, 800, 600);
    let duration = start.elapsed();

    // Log metrics
    metrics::histogram!("render_duration_ms", duration.as_millis() as f64);

    match &result {
        Ok(_) => {
            metrics::increment_counter!("render_success");
        }
        Err(e) => {
            metrics::increment_counter!("render_error");
            eprintln!("Render error: {:?}", e);

            // Alert if error rate > threshold
            if get_error_rate() > 0.05 {
                send_alert("V2 error rate > 5%, consider rollback");
            }
        }
    }

    result
}
```

### Rollback Checklist

If V2 has critical issues:

- [ ] Set `FASTRENDER_USE_V1=1` environment variable
- [ ] Deploy with V1 fallback enabled
- [ ] Monitor error rates drop
- [ ] Investigate V2 issue
- [ ] Fix V2 issue
- [ ] Test fix
- [ ] Gradually re-enable V2 (10% → 50% → 100%)

## Common Migration Issues

### Issue 1: Missing Dimensions

**Problem:**
```rust
// This doesn't work in V2
let image = renderer.render(html)?;
//                          ^^^^ Missing width/height
```

**Solution:**
```rust
// Add explicit dimensions
let image = renderer.render(html, 800, 600)?;
```

### Issue 2: Font Loading Errors

**Problem:**
```
Error: Font("No fonts available")
```

**Solution:**
```rust
// Configure font fallback
let font_config = FontConfig::system_fonts()
    .add_fallback("DejaVu Sans")
    .add_fallback("Liberation Sans")
    .add_fallback("FreeSans");

let renderer = Renderer::builder()
    .font_config(font_config)
    .build();
```

### Issue 3: Different Output

**Problem:** V2 output looks different from V1

**Diagnosis:**
1. Check if V1 had bugs (e.g., table layout)
2. Verify CSS is valid
3. Check for CSS variable usage
4. Compare text rendering

**Solution:** V2 is more correct - update tests to expect V2 output

### Issue 4: Performance Regression

**Problem:** V2 slower than V1 for specific content

**Diagnosis:**
```rust
// Profile both versions
let start = Instant::now();
let v1_image = renderer_v1.render(html, 800, 600)?;
println!("V1: {:?}", start.elapsed());

let start = Instant::now();
let v2_image = renderer_v2.render(html, 800, 600)?;
println!("V2: {:?}", start.elapsed());
```

**Solution:**
- V2 text shaping is slower but more correct
- V2 should be faster for large pages (incremental layout)
- Report performance issues to FastRender team

## Support and Resources

### Documentation

- **V2 API Documentation:** https://docs.rs/fastrender/2.0
- **Migration Guide:** This document
- **API Design:** `docs/plan/11-api-design.md`
- **Feature Roadmap:** `docs/plan/05-css-features-roadmap.md`

### Community

- **GitHub Issues:** https://github.com/fastrender/fastrender/issues
- **Discussions:** https://github.com/fastrender/fastrender/discussions
- **Discord:** https://discord.gg/fastrender

### Getting Help

1. **Check this migration guide** for common issues
2. **Search GitHub issues** for similar problems
3. **Create issue** with minimal reproduction
4. **Include:** V1/V2 comparison, HTML/CSS, error messages

### Reporting Migration Issues

When reporting issues during migration:

```markdown
**Environment:**
- FastRender version: 2.0.0
- Rust version: 1.75.0
- OS: Linux/macOS/Windows

**Issue:**
- Using V2 results in [describe issue]
- V1 works correctly

**Reproduction:**
```html
<!-- Minimal HTML that reproduces issue -->
<div>...</div>
```

**Expected:** [What V1 did]
**Actual:** [What V2 does]

**Comparison:**
- V1 output: [screenshot/description]
- V2 output: [screenshot/description]
```

## FAQ

### Q: When should I migrate to V2?

**A:** Migrate when V2 reaches feature parity (Milestone 4, ~Month 8). If you need correct table layout, migrate at Milestone 3.

### Q: Can I run V1 and V2 together?

**A:** Yes, enable the `dual` feature flag and use `Renderer::new_v1()` vs `Renderer::new()`.

### Q: Will V1 receive bug fixes?

**A:** Only security fixes after V2 becomes stable. New features are V2-only.

### Q: How long will V1 be supported?

**A:** V1 will be available for ~18 months after V2 stable release, then removed.

### Q: Is V2 faster than V1?

**A:** For most content, similar. V2 is slower for text (proper shaping) but faster for large pages (incremental layout). Tables are fixed but not necessarily faster.

### Q: Do I need to change my HTML/CSS?

**A:** No, HTML/CSS is unchanged. Only the Rust API changes.

### Q: What if I find a V2 bug?

**A:** Report it on GitHub. Use V1 fallback while bug is fixed.

### Q: Can I contribute to V2 development?

**A:** Yes! See `CONTRIBUTING.md`. Focus areas: table layout, text rendering, CSS features.

## Acceptance Criteria

- [ ] V1 and V2 can coexist via feature flags
- [ ] Migration guide is comprehensive and tested
- [ ] All API changes are documented with examples
- [ ] Performance comparison shows V2 is acceptable
- [ ] Rollback procedures are tested
- [ ] Common migration issues are documented
- [ ] Migration checklist covers all steps
- [ ] Test suite validates both V1 and V2
- [ ] Timeline is realistic and communicated
- [ ] Support channels are ready for migration questions

## Timeline Estimates

| Phase | Duration | Effort | Dependencies |
|-------|----------|--------|--------------|
| Migration guide creation | 1 week | 1 engineer | V2 API design |
| Dual-version infrastructure | 1 week | 1 engineer | Feature flags |
| Migration tooling | 2 weeks | 1 engineer | V2 API stable |
| Documentation | 2 weeks | 1 engineer + tech writer | All above |
| Beta testing | 4 weeks | Community | V2 feature complete |
| Production rollout | 8 weeks | Gradual | Beta success |

**Total:** ~4-5 months from V2 feature-complete to V1 deprecation

## Next Steps

1. **Monitor V2 Development** - Track progress toward feature parity
2. **Prepare for Migration** - Audit HTML/CSS usage, identify workarounds
3. **Test V2 Early** - Try beta releases, report issues
4. **Plan Timeline** - Schedule migration during low-traffic period
5. **Train Team** - Ensure developers understand V2 API
6. **Update CI/CD** - Add V2 testing to pipelines

## References

- **V2 Architecture:** `docs/plan/00-architecture-decisions.md`
- **API Design:** `docs/plan/11-api-design.md`
- **Feature Roadmap:** `docs/plan/05-css-features-roadmap.md`
- **Testing Strategy:** `docs/plan/06-testing-strategy.md`
- **Taffy Integration:** https://github.com/DioxusLabs/taffy

---

**Last Updated:** 2025-11-19
**Status:** Living Document
**Maintained By:** FastRender Team
**Review Frequency:** Monthly during V2 development
