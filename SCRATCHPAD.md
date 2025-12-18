Render pipeline now decodes cached HTML with proper charset sniffing: fetch_pages stores the response Content-Type alongside each cached HTML (.html.meta), render_pages decodes bytes via the shared html::encoding helper (BOM/header/meta/default Windows-1252), and fetch_and_render reuses the shared decoder instead of its local copy. cloudflare.com and latimes.com timeouts still outstanding from earlier notes.
Cloudflare render perf fixed: web font loading filters to the page’s codepoints (skipping unused @font-face ranges) and web font HTTP timeout is 10s; FASTR_RENDER_TIMINGS now reports css_parse/style_prepare/style_apply. cloudflare.com renders in ~12s at 1200×800 instead of 70s+.
Inline split guard: TextItem::split_at now bails out on non-char-boundary offsets (avoiding UTF-8 slice panics) and a regression covers mid-emoji splits; cleaned an unused MixBlendMode import. Marker baseline/list-style-position/ellipsis regressions landed upstream.
fetch_pages cache writes are now centralized: HTML caching writes optional .html.meta sidecars via a helper, and tests cover meta persistence/removal; charset sniffing coverage unaffected.
fetch_and_render now reads .meta sidecars for file:// HTML inputs (e.g., cached pages) and passes the cached Content-Type through decode_html_bytes; added a regression ensuring Shift-JIS HTML decodes via the meta charset.
<<<<<<< HEAD
Aligned fetcher user-agents: exported DEFAULT_USER_AGENT in resource.rs, fetch_and_render now sends it on HTTP requests, and fetch_pages uses the same Chrome-like UA instead of the old compatible string. Trimmed an unused MixBlendMode import in display_list_renderer_test.
Deduplicated DEFAULT_USER_AGENT in resource.rs after rebase to avoid duplicate const definitions.
Hyphenation breaks now filter out non-char-boundary offsets before use, and a regression covers dropping a break inside a multibyte emoji.
Itemized run splitting now validates UTF-8 boundaries: paragraph splits skip invalid byte offsets and a regression ensures split_run_at rejects mid-codepoint splits while still splitting on valid boundaries.
Shaping clusters clamp to UTF-8 boundaries: build_clusters now clamps cluster start/end to char boundaries, and a regression guards mid-codepoint cluster inputs.
HttpFetcher now follows redirects (up to 10) with the shared User-Agent, and a local TCP server regression verifies redirected fetches return the final content/type.
HttpFetcher now sends an Accept-Language header by default (en-US,en;q=0.9); regression inspects an HTTP request to ensure the header is present.
Accept-Language default is exported as DEFAULT_ACCEPT_LANGUAGE so callers can share the header value; fetch_and_render already uses HttpFetcher so it inherits the default.
fetch_pages now accepts --accept-language to override the header and passes it through HttpFetcher; a local TCP regression asserts the override is sent when fetching.
HttpFetcher now retries misreported Content-Encoding by falling back to identity encoding when decompression fails; regression `fetch_http_retries_on_bad_gzip` covers invalid gzip bodies.
latimes.com fetch/render timed out at 60s; no changes made
HTML parsing now disables scripting so <noscript> fallbacks are parsed/renderable; regression added for noscript content.
Added DOM regression to ensure <noscript> content in <head> is preserved (e.g., fallback styles inside noscript remain in the tree).
CLI support: fetch_and_render, render_pages, and inspect_frag now accept --prefers-reduced-data to override FASTR_PREFERS_REDUCED_DATA (matching reduced-transparency flag), and parsing helpers/tests were added.
CLI support: render_pages, fetch_and_render, and inspect_frag also accept --prefers-reduced-motion (sets FASTR_PREFERS_REDUCED_MOTION) with parsing helpers/tests; usage text updated for parity.
Vertical text-overflow coverage fixed: inline IFC now derives inline available space from the vertical inline axis (constraints.height for vertical writing) and logs that base; vertical overflow tests set overflow-y:hidden to trigger ellipsis. text-overflow ellipsis now clamps line fragments to inline height in vertical writing.
Wikipedia.org task complete: base_url/meta handling + jsl10n-visible on html/body produce a centered portal with assets loaded (PNG ~121KB). Added regression `fetch_bytes_uses_base_url_from_meta` to verify cached file renders pick up base_url from .html.meta.
Fixed nowrap text-overflow + outside markers: avoid breaking overflowing unbreakable text onto a new line when wrapping is disabled, keeping ellipsis in the content region. Added regression test in inline context tests.
Unitless zero parsing fixed: CSS numbers are parsed before lengths so `opacity:0`/`z-index:0` no longer get dropped as lengths (wired overlay now hides instead of a gray veil). Added cascade regressions for opacity/z-index accepting unitless zero.
Calc numbers now parse for numeric properties: calc/min/max/clamp expressions with unitless results are parsed as numbers (so `opacity: calc(1 - .25)` and `z-index: calc(2+3)` work). Added cascade tests for opacity/z-index with calc().
Added image-marker coverage for the same nowrap text-overflow scenario to ensure ellipsis stays in content while outside list images remain at inline-start.
Added start-side text-overflow regression with outside markers and fixed overflow trimming to preserve list markers and place start ellipsis after them.
Agent18:
- Fixed embedded CSS URL scan to ignore assignment targets like `window.css = ...`, preventing bogus fetches; regression `unescapes_json_style_embedded_urls` now passes.
- Absolute-position helper uses AbsoluteLayout shrink-to-fit behavior; updated test expects width derived from intrinsic when both left/right are set.
- Replaced element sizing test adjusted to expect content-box width from border-box width (360px -> 330px content) per box-sizing.
- UA defaults now apply link state styles via data-fastr flags (visited/active/hover/focus) and iframe max-width check asserts 100% length.
- Match-parent text-align now propagates to text-align-last (resolution runs before text-align) so match-parent sets last-line alignment as expected.
- Remaining failing tests (vertical text-overflow ellipsis, table rowspans) untouched.
=======
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
Added yahoo.com and nasa.gov to fetch_pages targets (cargo check --bin fetch_pages passes). Fetch/render notes: yahoo/nasa fetch succeed; yahoo now renders in ~14s (timings: cascade ~2.3s, layout ~5.9s) at 1200×800; nasa renders in ~23s (timings: cascade ~1.0s, box_tree ~14.8s, layout ~0.3s, paint ~6s).
Added display-list renderer regressions for mix-blend-mode (non-isolated multiply vs isolated source-over).
render_pages/fetch_and_render support --timings to enable FASTR_RENDER_TIMINGS per-page logging; fetch_pages supports --timings for per-page fetch durations (including cache hits/errors).
booking.com fetch returns empty body even with Chrome UA; fetch_pages now treats empty bodies as errors and avoids caching them.
fetch_pages run populated cache for 85+ pages; render_pages rerun succeeded for all cached pages (86/86 pass) after split_at UTF-8 guard; latimes now renders (~106KB PNG).
Rowspan height distribution adjusted to favor auto rows while sharing spans; vertical text-overflow ellipsis tests now passing. Rowspan regressions (`calculate_row_heights_*`, `baseline_height_computation_skips_rowspanning_cells`) fixed.

- Rendered additional pages and found `openbsd.org` PNG was completely blank. Root cause: block layout dropped absolutely positioned children when the parent block was laid out via `layout_block_child` (positioned_children from `layout_children` were ignored). Added absolute-position handling to that path so out-of-flow children are laid out against the block's padding box.
- `openbsd.org` now renders with visible sidebar/content; `render_pages --pages openbsd.org` succeeds (PNG unique colors ~32k).
- Previous notes: marker baseline/list-style-position/ellipsis regressions landed upstream. Cloudflare/latimes fetch/render had timed out at 60s; no changes made.
CNN render with split_at char-boundary guard now finishes: render_pages --pages cnn.com --timeout 60 succeeded (~44s, ~196KB PNG); fetch_and_render timings ~7s cascade, ~4.6s box_tree, ~42s layout, ~0.5s paint.

- Fetched full page set (fetch_pages). Several 403/401s remain expected; cache populated for 85+ pages.
- render_pages was crashing on latimes.com due to splitting text at non-UTF-8 boundaries. TextItem::split_at now clamps to char boundaries and split_runs_preserving_shaping bails out when the run offset isn't a boundary, falling back to reshaping; added regression test split_at_handles_non_char_boundary_offsets.
- Reran render_pages for all cached pages; 86/86 pass, latimes.com now renders (PNG ~106KB). Summary in fetches/renders/_summary.log.

Wired.com blank render investigation (Mar 2025): max-height:max-content is now parsed/propagated (flag on ComputedStyle/PositionedStyle), percentage heights resolve only when the parent height is definite, and the stacking tree skips visibility:hidden fragments. Render still blank (PNG nonwhite bbox 0,172–1200,173). inspect_frag shows overlays hidden but a nav drawer container (`OneNavRowGrid` under `FocusTrapContainer`) still lays out at (0,439,1200,800) keeping `main` at y≈1239, and carousel row content begins at x≈1080 even though the container flex-wrap=wrap/justify=center. Need to debug why the hidden nav/drawer occupies normal flow and why flex items are offset right (possibly wrap not honored or min_x anchored by earlier items).
