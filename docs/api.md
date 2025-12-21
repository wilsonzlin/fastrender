# Library API (overview)

The public Rust API is exported from `src/lib.rs` and implemented primarily in `src/api.rs`.

## Minimal example

```rust
use fastrender::FastRender;

let html = "<html><body><h1>Hello</h1></body></html>";

let mut renderer = FastRender::new()?;
let pixmap = renderer.render_html(html, 800, 600)?;
pixmap.save_png("out.png")?;
```

## Common helpers

- `FastRender::render_to_png(html, width, height)` returns encoded PNG bytes
- `FastRender::render_to_jpeg(html, width, height, quality)`
- `FastRender::render_to_webp(html, width, height, quality)`
