w3.org grid render fixed: grid layout now falls back to stacking children when Taffy reports zero-width grid items, forcing containers to fill the available width; added env logging FASTR_LOG_GRID_ROOT/FASTR_LOG_GRID_CHILDREN. Added w3.org to fetch_pages targets. Regression `grid_children_are_laid_out_even_when_taffy_reports_zero_width` guards layout of grid children; local patches previously under /tmp/w3-grid-patches.

Render pipeline now decodes cached HTML with proper charset sniffing: fetch_pages stores the response Content-Type alongside each cached HTML (.html.meta), render_pages decodes bytes via the shared html::encoding helper (BOM/header/meta/default Windows-1252), and fetch_and_render reuses the shared decoder instead of its local copy. cloudflare.com timeout still outstanding; latimes.com now renders in ~27s at 1200×800 with current defaults.
Cloudflare render perf fixed: web font loading filters to the page’s codepoints (skipping unused @font-face ranges) and web font HTTP timeout is 10s; FASTR_RENDER_TIMINGS now reports css_parse/style_prepare/style_apply. cloudflare.com renders in ~12s at 1200×800 instead of 70s+.
Inline split guard: TextItem::split_at now bails out on non-char-boundary offsets (avoiding UTF-8 slice panics) with regressions for mid-emoji and `previous_char_boundary_in_text`; marker baseline/list-style-position/ellipsis regressions landed upstream. Split-at safety now returns None on non-character-boundary offsets (no silent clamping).
render_pages CLI now accepts --accept-language (default en-US,en;q=0.9); HttpFetcher uses the provided value alongside User-Agent when fetching linked assets, and usage/help logs the chosen header.
fetch_pages cache writes are now centralized: HTML caching writes optional .html.meta sidecars via a helper, and tests cover meta persistence/removal; charset sniffing coverage unaffected.
fetch_and_render now reads .meta sidecars for file:// HTML inputs (e.g., cached pages) and passes the cached Content-Type through decode_html_bytes; added a regression ensuring Shift-JIS HTML decodes via the meta charset.
Aligned fetcher user-agents: exported DEFAULT_USER_AGENT in resource.rs, fetch_and_render now sends it on HTTP requests, and fetch_pages uses the same Chrome-like UA instead of the old compatible string. Deduplicated DEFAULT_USER_AGENT after rebase.
Hyphenation breaks now filter out non-char-boundary offsets before use; itemized run splitting validates UTF-8 boundaries; shaping clusters clamp to UTF-8 boundaries with regression coverage.
HttpFetcher now follows redirects (up to 10) with the shared User-Agent; sends Accept-Language by default; retries misreported Content-Encoding by falling back to identity (`fetch_http_retries_on_bad_gzip`).
fetch_pages accepts --accept-language to override the header; treats empty HTTP bodies as errors (regression added) and reports failed URLs in the summary output.
latimes.com fetch/render previously timed out at 60s; rerendered successfully in ~27s (HTML cached under fetches/html/latimes.com.html). latimes.com UTF-8 boundary panic fixed via text split clamping (renders ≈72s at 1200×800 after fixes).
HTML parsing now disables scripting so <noscript> fallbacks are parsed/renderable; regressions ensure <noscript> content in <head> is preserved. fetch_and_render/render_pages/inspect_frag accept --prefers-reduced-data and --prefers-reduced-motion flags.
Bug hunt: github.com render appears dark but matches dark landing design (hero text/buttons visible near y≈200 in inspect_frag); likely no layout issue.
Vertical text-overflow coverage fixed for vertical writing; line fragments clamp to inline height.
Wikipedia portal: base_url/meta fixes + jsl10n-visible injection for html/body produce centered portal with assets loaded (PNG ~121KB). Unit test `fetch_bytes_uses_base_url_from_meta` added.
Fixed nowrap text-overflow + outside markers; added regressions for outside markers, start-side ellipsis, and image markers.
Unitless zero parsing fixed; calc/min/max/clamp parsing supports numeric properties (opacity/z-index/order) with regressions. Order property added to KNOWN_PROPERTIES; non-integer calc orders are ignored per spec.
Container queries respect inline-size without requiring a block dimension; optional logging via FASTR_LOG_CONTAINER_QUERY.
Added yahoo.com and nasa.gov to fetch_pages targets (cargo check --bin fetch_pages passes). Fetch/render notes: yahoo/nasa fetch succeed but render timed out at 60s (heavy pages); yahoo renders in ~113s with timings under 120s timeout.
Added display-list renderer regressions for mix-blend-mode (non-isolated multiply vs isolated source-over).
render_pages/fetch_and_render support --timings to enable FASTR_RENDER_TIMINGS per-page logging; fetch_pages supports --timings for per-page fetch durations (including cache hits/errors).
booking.com fetch returns empty body even with Chrome UA; fetch_pages now treats empty bodies as errors and avoids caching them.
fetch_pages run populated cache for 85+ pages; render_pages rerun succeeded for all cached pages (86/86 pass) after split_at UTF-8 guard; latimes now renders (~106KB PNG).
Rowspan height distribution adjusted to favor auto rows while sharing spans; vertical text-overflow ellipsis tests now passing. Rowspan regressions (`calculate_row_heights_*`, `baseline_height_computation_skips_rowspanning_cells`) fixed.

- Rendered additional pages and found `openbsd.org` PNG was completely blank. Root cause: block layout dropped absolutely positioned children when the parent block was laid out via `layout_block_child` (positioned_children from `layout_children` were ignored). Added absolute-position handling to that path so out-of-flow children are laid out against the block's padding box.
- `openbsd.org` now renders with visible sidebar/content; `render_pages --pages openbsd.org` succeeds (PNG unique colors ~32k).
- Added regression `absolute_children_inside_block_descendants_are_laid_out` covering positioned children under nested blocks to guard the fix.
- Previous notes: marker baseline/list-style-position/ellipsis regressions landed upstream. Cloudflare fetch/render had timed out at 60s; no changes made.
CNN render with split_at char-boundary guard now finishes: render_pages --pages cnn.com --timeout 60 succeeded (~44s, ~196KB PNG); fetch_and_render timings ~7s cascade, ~4.6s box_tree, ~42s layout, ~0.5s paint.
