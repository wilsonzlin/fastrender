# Empty `url("")` tokens and image placeholder heuristics

Some pageset sites emit CSS and image URLs that look “valid” syntactically but are effectively
non-functional at runtime. In a browser these often turn into a redundant fetch (typically of the
current document URL), while in FastRender they can produce noisy subresource diagnostics that
cause pages to report `ok with failures`.

This note documents the project’s current policy for keeping the pageset scoreboard signal-heavy.

## 1) Treat `url("")` as `none` (or invalid) for image-like CSS properties

Real-world CSS occasionally contains `url("")` (or `url(' ')`) in properties that expect an image
value. When resolved against a document base URL, the empty URL typically points back at the
document itself (HTML), which isn’t a usable image.

FastRender treats empty URLs as non-images to avoid:

- attempting to fetch a meaningless URL, and
- recording noisy fetch/decode diagnostics that don’t materially affect rendering.

This behavior currently applies to:

- `background-image` / background layer images
- `background` shorthand (image component)
- `image-set(...)` candidates that are `url("")` (those candidates are ignored)
- `cursor` images
- `list-style-image` (including `image-set(...)`)
- `filter: url("")` (treated as invalid rather than fetching)

Implementation: `src/style/properties.rs`.

## 2) Treat some “successful but not an image” responses as transparent placeholders

Some pages embed tracking pixels or other subresources that:

- return an empty HTTP body with a 2xx status, or
- return HTML/markup (sometimes via range requests, i.e. `206 Partial Content`) even though the URL
  is being used as an image.

Feeding these payloads into image probing/decoding produces diagnostics like “Unable to determine
image dimensions”, which previously counted as a non-fatal pageset failure.

FastRender now recognizes these cases and returns a cached 1×1 transparent placeholder **without
recording a diagnostics error**, so layout/paint can proceed deterministically without degrading the
pageset scoreboard.

Implementation: `src/image_loader.rs`.

