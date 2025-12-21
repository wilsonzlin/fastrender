# CSS loading & URL resolution

FastRender extracts CSS from:

- Inline `<style>` blocks in the document
- Linked stylesheets (`<link rel="stylesheet" href="…">`)
- `@import` rules inside stylesheets

The core implementation is in `src/css/loader.rs`.

## Link resolution rules (high level)

When collecting and resolving stylesheet URLs, FastRender:

- Trims and ignores empty/whitespace-only hrefs.
- Rejects `javascript:`, `mailto:`, and `vbscript:` URLs.
- Unescapes JavaScript-escaped URLs before resolution (e.g. `https:\\u002f\\u002fexample.com`).
- Preserves `data:` URLs.
- Ignores fragment-only hrefs (e.g. `#section`) when collecting linked CSS.
- Supports `file://` bases for both directories (implicit trailing slash) and files (relative paths resolve against the parent directory).
- Preserves scheme-relative URLs (`//example.com/...`) by resolving them against the base scheme.

See unit tests in `src/css/loader.rs` for edge-case coverage.
