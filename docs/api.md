# Library API (overview)

The public Rust API is exported from `src/lib.rs` and implemented primarily in `src/api.rs`.

## Creating a renderer

```rust
use fastrender::FastRender;

let mut renderer = FastRender::new()?;
let pixmap = renderer.render_html("<h1>Hello</h1>", 800, 600)?;
pixmap.save_png("out.png")?;
```

Use `FastRender::builder()` to set defaults (viewport size, background color, base URL, etc.) when constructing a renderer.

## Per-request render options

`RenderOptions` overrides defaults for a single render without mutating the renderer:

- `viewport: Option<(u32, u32)>` – override the viewport for this render (falls back to the renderer’s `default_width`/`default_height`).
- `device_pixel_ratio: Option<f32>` – temporarily override the DPR used for media queries and image/srcset selection.
- `media_type: MediaType` – defaults to `MediaType::Screen`.
- `scroll_x`, `scroll_y` – apply CSS px scroll offsets before painting.
- `allow_partial` – when rendering URLs, return a placeholder pixmap + diagnostics instead of an error if the document fails to fetch.

```rust
use fastrender::{FastRender, RenderOptions};

let mut renderer = FastRender::new()?;
let options = RenderOptions::new()
    .with_viewport(1280, 720)
    .with_device_pixel_ratio(2.0)
    .with_scroll(0.0, 320.0);

let pixmap = renderer.render_html_with_options("<p>Scrolled</p>", options)?;
```

## Prepared documents for repeated painting

`FastRender::prepare_html` runs parse/style/layout once and returns a
`PreparedDocument` containing the DOM, resolved stylesheet (including `@import`s),
styled tree, box tree, and laid-out fragment tree. Painting uses the same font
context and image cache, so you can render multiple scroll offsets or animation
timelines without re-running expensive stages.

```rust
use fastrender::{FastRender, RenderOptions};

let mut renderer = FastRender::new()?;
let session = renderer.prepare_html(
    "<div class='hero'></div>",
    RenderOptions::new().with_viewport(800, 600),
)?;

// Paint the default scroll position
let initial = session.paint_default()?;

// Paint a scrolled view with a darker background
let scrolled = session.paint(0.0, 200.0, None, Some(fastrender::Rgba::BLACK))?;
```

`PreparedDocument::paint` accepts optional viewport and background overrides, and
`paint_region` is a convenience for tiled rendering. `PreparedDocument` is `Send`
but not `Sync`; create one session per thread when painting concurrently.

## Rendering URLs and diagnostics

`FastRender::render_url`/`render_url_with_options` return a `RenderResult` containing the rendered `Pixmap` and any `RenderDiagnostics` captured while fetching resources. Subresource fetch failures (e.g. linked CSS) are recorded in `diagnostics.fetch_errors` but do not abort the render.

When `RenderOptions::allow_partial(true)` is set, a failed document fetch records `diagnostics.document_error` and returns a placeholder pixmap instead of an error. Otherwise the fetch failure is returned as `Error::Navigation`.

```rust
use fastrender::{FastRender, RenderOptions};

let mut renderer = FastRender::new()?;
let result = renderer.render_url_with_options(
    "https://example.com",
    RenderOptions::new().allow_partial(true),
)?;

if let Some(reason) = &result.diagnostics.document_error {
    eprintln!("document fetch failed: {reason}");
}
for error in &result.diagnostics.fetch_errors {
    eprintln!("[{:#?}] {}: {}", error.kind, error.url, error.message);
}

result.pixmap.save_png("example.png")?;
```

`RenderResult::into_pixmap()` discards diagnostics when they are not needed.

## DOM compatibility mode

`DomCompatibilityMode` controls whether FastRender applies small, JS-era DOM mutations while parsing. The default (`Standard`) leaves the parsed DOM untouched; `Compatibility` applies class flips used by some sites that gate content on “JS enabled” markers (see `docs/notes/dom-compatibility.md`).

Configure it when building a renderer:

```rust
use fastrender::{FastRender, FastRenderConfig};
use fastrender::dom::DomCompatibilityMode;

let config = FastRenderConfig::new()
    .with_dom_compat_mode(DomCompatibilityMode::Compatibility);
let mut renderer = FastRender::with_config(config)?;
```

The builder helper `FastRenderBuilder::dom_compatibility_mode` is equivalent.

## Common helpers

- `FastRender::render_to_png(html, width, height)` returns encoded PNG bytes
- `FastRender::render_to_jpeg(html, width, height, quality)`
- `FastRender::render_to_webp(html, width, height, quality)` (quality is clamped to
  0–100; a value of 100 produces lossless WebP output)
