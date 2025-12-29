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

## Resource fetch policy

Resource loading is controlled by a `ResourcePolicy` (`FetchPolicy` is an alias) configured on `FastRenderConfig` or via `FastRenderBuilder::resource_policy(...)`. The default mirrors the historical behavior: allow `http`, `https`, `file`, and `data` URLs; a 50 MB per-response cap; a 30 second timeout; up to 10 redirects; and no host allow/deny rules or total budget.

Key knobs:

- `allowed_schemes` – enable/disable `http`, `https`, `file`, `data`
- `max_response_bytes` – per-resource cap (responses larger than this are rejected)
- `total_bytes_budget` – overall byte budget across all fetched resources
- `request_timeout` – per-request timeout
- `max_redirects` – maximum requests when following redirects
- `host_allowlist` / `host_denylist` – optional hostname filters for HTTP(S)

```rust
use fastrender::{FastRender, ResourcePolicy};

let policy = ResourcePolicy::default()
    .allow_http(false) // block http: loads
    .with_total_bytes_budget(Some(5 * 1024 * 1024)); // 5 MB total budget

let mut renderer = FastRender::builder()
    .resource_policy(policy)
    .build()?;
```

Resource fetches made by the library and CLI tools default to the in-memory
[`CachingFetcher`](../src/resource.rs). It performs single-flight deduplication and
keeps an LRU cache of responses. When `CachingFetcherConfig::honor_http_cache_headers`
is enabled (the default), ETag/Last-Modified validators are used for HTTP
revalidation. Set `CachingFetcherConfig::honor_http_cache_freshness` to respect
`Cache-Control`/`Expires` freshness directives: fresh entries are served from
cache without a network request, while stale or `no-cache`/`must-revalidate`
responses trigger conditional requests when validators are present. `Cache-Control:
no-store` responses are never cached (in-memory or on disk).

The optional `DiskCachingFetcher` persists the same metadata on disk. Its
`DiskCacheConfig::max_age` value caps freshness even when servers advertise
longer lifetimes.

Fetches blocked by policy (disallowed scheme/host, over budget, etc.) are recorded in `RenderDiagnostics.fetch_errors` when rendering URLs.

`FastRenderConfig` also carries a `ResourceAccessPolicy` for subresource access. By default it mirrors browser defaults (block `file://` from HTTP(S) documents, allow cross-origin loads). Set `with_block_mixed_content(true)` to forbid HTTP under HTTPS; `with_same_origin_subresources(true)` to reject cross-origin CSS/images/fonts when the document origin is known; and `with_allowed_subresource_origins(...)` to permit an explicit allowlist alongside the document origin.

## Per-request render options

`RenderOptions` overrides defaults for a single render without mutating the renderer:

- `viewport: Option<(u32, u32)>` – override the viewport for this render (falls back to the renderer’s `default_width`/`default_height`).
- `device_pixel_ratio: Option<f32>` – temporarily override the DPR used for media queries and image/srcset selection.
- `diagnostics_level: DiagnosticsLevel` – enable structured diagnostics (`None`/`Basic`/`Verbose`).
- `media_type: MediaType` – defaults to `MediaType::Screen`.
- `scroll_x`, `scroll_y` – apply CSS px scroll offsets before painting.
- `element_scroll_offsets` – map element scroll containers (by box ID) to scroll offsets.
- `css_limit: Option<usize>` – limit the number of linked stylesheets inlined by the fetch helpers.
- `capture_accessibility: bool` – include an accessibility tree in render results (see below).
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

Element scroll offsets can be supplied via `RenderOptions::with_element_scroll_offsets` or
`with_element_scroll` using box IDs from a `PreparedDocument::box_tree()` (or the `inspect_frag`
CLI) to render scrolled containers without re-running layout.

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

// Sample animations at a specific time (milliseconds since load) without re-running layout
let animated = session.paint_at_time(500.0)?;
```

`PreparedDocument::paint` accepts optional viewport and background overrides, and
`paint_region` is a convenience for tiled rendering. `PreparedPaintOptions` exposes
full scroll state (including element scroll offsets) and animation time overrides in milliseconds
when repainting without re-running layout, and `paint_at_time` is a shorthand for sampling
time-based animations. `PreparedDocument` is `Send`
but not `Sync`; create one session per thread when painting concurrently.

## Rendering URLs and diagnostics

`FastRender::render_url`/`render_url_with_options` return a `RenderResult` containing the rendered `Pixmap` and any `RenderDiagnostics` captured while fetching resources. Subresource fetch failures (e.g. linked CSS) are recorded in `diagnostics.fetch_errors` but do not abort the render.

When `RenderOptions::allow_partial(true)` is set, a failed document fetch records `diagnostics.document_error` and returns a placeholder pixmap instead of an error. Otherwise the fetch failure is returned as `Error::Navigation`.

```rust
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

let mut renderer = FastRender::new()?;
let result = renderer.render_url_with_options(
    "https://example.com",
    RenderOptions::new()
        .allow_partial(true)
        .with_diagnostics_level(DiagnosticsLevel::Basic),
)?;

if let Some(reason) = &result.diagnostics.document_error {
    eprintln!("document fetch failed: {reason}");
}
for error in &result.diagnostics.fetch_errors {
    eprintln!("[{:#?}] {}: {}", error.kind, error.url, error.message);
}
if let Some(stats) = &result.diagnostics.stats {
    eprintln!("DOM nodes: {:?}", stats.counts.dom_nodes);
    eprintln!("Paint time (ms): {:?}", stats.timings.paint_rasterize_ms);
}

result.pixmap.save_png("example.png")?;
```

`RenderResult::encode` returns encoded bytes while annotating `timings.encode_ms` when diagnostics are present:

```rust
let (png, diagnostics) = result.encode(fastrender::OutputFormat::Png)?;
```

`RenderResult::into_pixmap()` discards diagnostics when they are not needed. To collect diagnostics when rendering raw HTML, use `render_html_with_diagnostics`.

## Iframe rendering depth

Iframe documents are rendered recursively up to `FastRenderConfig::max_iframe_depth` levels (default:
3). Configure it via `FastRenderConfig::with_max_iframe_depth` or the builder helper:

```rust
let mut renderer = FastRender::builder()
    .max_iframe_depth(1) // render only direct iframe children
    .build()?;
```

Semantics:
- `max_iframe_depth = 0` disables iframe content rendering entirely.
- Values greater than zero allow up to that many nested iframe levels from the current document
  (each iframe document inherits the parent’s depth minus one).
- The display-list paint backend currently emits placeholders for iframes instead of painting their
  nested content.

## Parallel rendering

`FastRender` instances are `Send` but not `Sync`, so callers should avoid sharing a single instance across threads.
Use `FastRenderPool` to render multiple pages concurrently while sharing immutable resources like fonts and the UA stylesheet:

```rust
use fastrender::api::{FastRenderPool, FastRenderPoolConfig};

let pool = FastRenderPool::with_config(
    FastRenderPoolConfig::new().with_pool_size(4)
)?;

let handles: Vec<_> = (0..4).map(|_| {
    let pool = pool.clone();
    std::thread::spawn(move || {
        pool.render_html("<h1>Hi!</h1>", 800, 600).unwrap()
    })
}).collect();

for handle in handles {
    let pixmap = handle.join().unwrap();
    // Save, compare, etc.
}
```

## Meta viewport handling

FastRender ignores `<meta name="viewport">` directives by default to keep layouts
stable across renders. Enable support explicitly when you want responsive pages
to honor author-provided viewport hints:

```rust
use fastrender::api::{FastRender, FastRenderConfig};

let mut renderer =
  FastRender::with_config(FastRenderConfig::new().with_meta_viewport(true))?;
```

When enabled:

- `width=device-width`/numeric `width`/`height` set the layout viewport used by
  CSS `vw`/`vh` and media queries.
- `initial-scale` sets the zoom factor; if omitted, a default scale is derived
  from the requested width/height when provided.
- `minimum-scale`/`maximum-scale` clamp the zoom; all zoom factors are limited to
  the 0.1–10 range for stability.
- The visual viewport (used for `device-width` media features) and effective
  device pixel ratio are scaled by the resolved zoom factor.

## Accessibility trees

FastRender can emit the computed accessibility tree so semantics can be validated alongside pixels:

- `FastRender::accessibility_tree_html(html, options)` builds the accessibility tree directly from HTML without running layout/paint.
- `FastRender::render_html_with_accessibility(html, options)` returns both the rendered `Pixmap` and the accessibility tree for convenience.
- When `RenderOptions::capture_accessibility` is set, `RenderResult` populates `accessibility: Option<AccessibilityNode>` alongside diagnostics.

`AccessibilityNode` is `Serialize`, so callers can convert it to `serde_json::Value` or a pretty-printed string. The `dump_a11y` CLI binary emits the JSON tree for a cached HTML document.

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

## Inspecting computed styles and layout

Use `FastRender::inspect` to query a document by CSS selector, id, or styled node id and
retrieve deterministic JSON-friendly snapshots of the matching nodes:

```rust,ignore
use fastrender::{FastRender, InspectQuery};

let mut renderer = FastRender::new()?;
let dom = renderer.parse_html("<div id='target'>Hello</div>")?;

let snapshots = renderer.inspect(&dom, 800, 600, InspectQuery::Id("target".into()))?;
println!("{}", serde_json::to_string_pretty(&snapshots)?);
```

Each snapshot includes:

- `node`: tag/attributes/ancestry for the matched DOM node
- `style`: a computed-style subset (display/position/float/overflow/box model, colors, font/line height)
- `boxes`: box tree metadata (box id, type, formatting context, debug selector)
- `fragments`: layout fragments with bounds/baseline indices and scroll container metadata

The `inspect_frag` CLI also supports this workflow:

```
cargo run --bin inspect_frag -- file.html --query "#main .item:nth-child(2)"
```
