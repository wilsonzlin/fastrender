# CSS loading & URL resolution

FastRender extracts CSS from:

- Inline `<style>` blocks in the document
- Linked stylesheets (`<link rel="stylesheet" href="…">`, plus `<link rel="preload" as="style" href="…">` when `FASTR_FETCH_PRELOAD_STYLESHEETS` is enabled; `modulepreload` is opt-in via `FASTR_FETCH_MODULEPRELOAD_STYLESHEETS`)
- `@import` rules inside stylesheets

In the render pipeline:

- Stylesheet *source extraction* happens in `src/css/parser.rs` (see `extract_scoped_css_sources`).
- Stylesheet *fetch + parse + @import resolution* happens in `FastRender::collect_document_style_set`
  (`src/api.rs`).
- URL resolution and `url(...)` rewriting helpers live in `src/css/loader.rs`.

FastRender also exposes `inline_stylesheets_*` helpers that explicitly fetch linked stylesheets and
inject a `<style>` block into HTML for tooling. These helpers are not used by the main render path.

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
- Disk caching keeps an `index.jsonl` journal alongside cached files so eviction can happen incrementally once the configured byte budget is exceeded, instead of rescanning the entire cache directory on every write. The index is rebuilt if missing or corrupt and removes any stray or partial cache files it finds.
- Cached HTTP responses are revalidated with `ETag`/`Last-Modified` when present; a `304 Not Modified` response refreshes the cached validators without dropping the stored bytes.
- Pageset-oriented wrappers (`cargo xtask pageset`, `scripts/pageset.sh`, `scripts/profile_*`) enable `disk_cache` by default so subsequent runs avoid refetching the same subresources and stay reproducible; pass `--no-disk-cache`/`DISK_CACHE=0` to force cold fetches.

Stylesheet fetches are best-effort: failures (network errors, blocked by `ResourcePolicy`, invalid
`@import` targets, etc.) are skipped while recording an entry in `RenderDiagnostics.fetch_errors`
when rendering URLs. Renderers built with `RenderOptions::allow_partial(true)` continue rendering the
rest of the page even when some stylesheets are blocked or missing.
