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
- Honors `<base href>` (including relative values). Trailing slashes are preserved so `/assets` and
  `/assets/` resolve differently.
- Supports `file://` bases for both directories (implicit trailing slash) and files (relative paths resolve against the parent directory).
- Preserves scheme-relative URLs (`//example.com/...`) by resolving them against the base scheme.
- Relative `url()` references inside imported stylesheets are rewritten against the imported sheet's
  own URL before inlining.
- Recursive `@import` inlining is capped to avoid unbounded fetches; cycles are reported via render
  diagnostics.

See unit tests in `src/css/loader.rs` for edge-case coverage.

## Caching and revalidation

- Fetches performed by the CLI and library default to [`fastrender::resource::CachingFetcher`], an in-memory LRU with single-flight semantics to avoid duplicate concurrent requests.
- When built with the optional `disk_cache` feature, subresource fetches can be wrapped in [`fastrender::resource::DiskCachingFetcher`], which persists bytes plus minimal metadata (content-type, validators, final URL) under `fetches/assets/`.
- Cached HTTP responses are revalidated with `ETag`/`Last-Modified` when present; a `304 Not Modified` response refreshes the cached validators without dropping the stored bytes.

Stylesheet fetches are best-effort: failures (network errors, blocked by `ResourcePolicy`, invalid
`@import` targets, etc.) are skipped while recording an entry in `RenderDiagnostics.fetch_errors`
when rendering URLs. Renderers built with `RenderOptions::allow_partial(true)` continue rendering the
rest of the page even when some stylesheets are blocked or missing.
