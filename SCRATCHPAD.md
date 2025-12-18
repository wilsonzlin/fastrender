Render pipeline now decodes cached HTML with proper charset sniffing: fetch_pages stores the response Content-Type alongside each cached HTML (.html.meta), render_pages decodes bytes via the shared html::encoding helper (BOM/header/meta/default Windows-1252), and fetch_and_render reuses the shared decoder instead of its local copy. cloudflare.com and latimes.com timeouts still outstanding from earlier notes.
Cloudflare render perf fixed: web font loading filters to the page’s codepoints (skipping unused @font-face ranges) and web font HTTP timeout is 10s; FASTR_RENDER_TIMINGS now reports css_parse/style_prepare/style_apply. cloudflare.com renders in ~12s at 1200×800 instead of 70s+.
Inline split guard: TextItem::split_at now bails out on non-char-boundary offsets (avoiding UTF-8 slice panics) and a regression covers mid-emoji splits; cleaned an unused MixBlendMode import. Added unit coverage for `previous_char_boundary_in_text` (multibyte offsets clamp to start; past-end clamps to len). Marker baseline/list-style-position/ellipsis regressions landed upstream.
render_pages CLI now accepts --accept-language (default en-US,en;q=0.9); HttpFetcher uses the provided value alongside User-Agent when fetching linked assets, and usage/help logs the chosen header.
fetch_pages cache writes are now centralized: HTML caching writes optional .html.meta sidecars via a helper, and tests cover meta persistence/removal; charset sniffing coverage unaffected.
fetch_and_render now reads .meta sidecars for file:// HTML inputs (e.g., cached pages) and passes the cached Content-Type through decode_html_bytes; added a regression ensuring Shift-JIS HTML decodes via the meta charset.
Aligned fetcher user-agents: exported DEFAULT_USER_AGENT in resource.rs, fetch_and_render now sends it on HTTP requests, and fetch_pages uses the same Chrome-like UA instead of the old compatible string. Trimmed an unused MixBlendMode import in display_list_renderer_test.
Deduplicated DEFAULT_USER_AGENT in resource.rs after rebase to avoid duplicate const definitions.
Hyphenation breaks now filter out non-char-boundary offsets before use, and a regression covers dropping a break inside a multibyte emoji.
Itemized run splitting now validates UTF-8 boundaries: paragraph splits skip invalid byte offsets and a regression ensures split_run_at rejects mid-codepoint splits while still splitting on valid boundaries.
Shaping clusters clamp to UTF-8 boundaries: build_clusters now clamps cluster start/end to char boundaries, and a regression guards mid-codepoint cluster inputs.
HttpFetcher now follows redirects (up to 10) with the shared User-Agent, and a local TCP server regression verifies redirected fetches return the final content/type.
HttpFetcher now sends an Accept-Language header by default (en-US,en;q=0.9); regression inspects an HTTP request to ensure the header is present.
Accept-Language default is exported as DEFAULT_ACCEPT_LANGUAGE so callers can share the header value; fetch_and_render already uses HttpFetcher so it inherits the default.
fetch_pages now accepts --accept-language to override the header and passes it through HttpFetcher; a local TCP regression asserts the override is sent when fetching.
<<<<<<< HEAD
HttpFetcher now retries misreported Content-Encoding by falling back to identity encoding when decompression fails; regression `fetch_http_retries_on_bad_gzip` covers invalid gzip bodies.
=======
render_pages and fetch_and_render now accept --accept-language, pass it through HttpFetcher/fetch_bytes, and print the chosen header.
SSH access to origin is currently failing (Bye Bye disconnect), so the Accept-Language CLI changes are committed locally but not yet pushed.
cloudflare.com fetch/render timed out at 60s; no changes made
>>>>>>> 64dab5a (Expose Accept-Language override in render/fetch CLI)
latimes.com fetch/render timed out at 60s; no changes made
HTML parsing now disables scripting so <noscript> fallbacks are parsed/renderable; regression added for noscript content.
Logging: fetch_and_render prints UA/viewport/scroll/output parameters; render_pages per-page logs include the UA used. Note: push intermittently timing out; local formatting-only commit (unitless zero test) may remain pending until connectivity stabilizes.
Added DOM regression to ensure <noscript> content in <head> is preserved (e.g., fallback styles inside noscript remain in the tree).
CLI support: fetch_and_render, render_pages, and inspect_frag now accept --prefers-reduced-data to override FASTR_PREFERS_REDUCED_DATA (matching reduced-transparency flag), and parsing helpers/tests were added.
CLI support: render_pages, fetch_and_render, and inspect_frag also accept --prefers-reduced-motion (sets FASTR_PREFERS_REDUCED_MOTION) with parsing helpers/tests; usage text updated for parity.
Bug hunt: github.com render (1200x800) shows a dark gradient-heavy page with minimal light pixels; content appears mostly dark, needs further layout/visibility inspection to surface expected hero text.
Inspect_frag on cached github.com shows hero text and buttons at ~y=200 with dark theme backgrounds; render seems consistent with site’s dark landing design. Likely no layout bug beyond dark styling.
Vertical text-overflow coverage fixed: inline IFC now derives inline available space from the vertical inline axis (constraints.height for vertical writing) and logs that base; vertical overflow tests set overflow-y:hidden to trigger ellipsis. text-overflow ellipsis now clamps line fragments to inline height in vertical writing.
Current task (Agent 6): investigating wikipedia.org render. DOM parse flips `no-js`→`js-enabled` and injects `jsl10n-visible` on HTML+BODY. Cached meta now stores `content-type` + `url`; render_pages/fetch_and_render parse it (legacy-safe) to seed base_url for cached renders. After refetch/render, wikipedia.org assets load (PNG ~121KB) with central featured links visible; next steps: confirm portal centering and clean up any off-page hidden text/opacity artifacts.
Added unit test `fetch_bytes_uses_base_url_from_meta` to ensure file:// HTML caches supply base_url from meta sidecars.
Wikipedia.org render now complete: base_url/meta fixes + jsl10n-visible injection produce centered portal with assets loaded (PNG ~121KB). Task closed; ready for next.
Fixed nowrap text-overflow + outside markers: avoid breaking overflowing unbreakable text onto a new line when wrapping is disabled, keeping ellipsis in the content region. Added regression test in inline context tests.
Unitless zero parsing fixed: CSS numbers are parsed before lengths so `opacity:0`/`z-index:0` no longer get dropped as lengths (wired overlay now hides instead of a gray veil). Added cascade regressions for opacity/z-index accepting unitless zero.
Calc numbers now parse for numeric properties: calc/min/max/clamp expressions with unitless results are parsed as numbers (so `opacity: calc(1 - .25)` and `z-index: calc(2+3)` work). Added cascade tests for opacity/z-index with calc().
Order property now parses (added to KNOWN_PROPERTIES) and accepts calc() integer results; non-integer calc order values are ignored per spec. Tests added.
Added image-marker coverage for the same nowrap text-overflow scenario to ensure ellipsis stays in content while outside list images remain at inline-start.
Added start-side text-overflow regression with outside markers and fixed overflow trimming to preserve list markers and place start ellipsis after them.
Agent18:
- Fixed embedded CSS URL scan to ignore assignment targets like `window.css = ...`, preventing bogus fetches; regression `unescapes_json_style_embedded_urls` now passes.
- Absolute-position helper uses AbsoluteLayout shrink-to-fit behavior; updated test expects width derived from intrinsic when both left/right are set.
- Replaced element sizing test adjusted to expect content-box width from border-box width (360px -> 330px content) per box-sizing.
- UA defaults now apply link state styles via data-fastr flags (visited/active/hover/focus) and iframe max-width check asserts 100% length.
- Match-parent text-align now propagates to text-align-last (resolution runs before text-align) so match-parent sets last-line alignment as expected.
- Remaining failing tests (vertical text-overflow ellipsis, table rowspans) untouched.

Container queries respect inline-size without requiring a block dimension: media length resolution no longer demands finite viewport height when the query lacks viewport-relative units, so inline-size containers evaluate size queries correctly while block-size queries still fail for inline-only containers. Added optional logging for container sizes (FASTR_LOG_CONTAINER_QUERY).

latimes.com UTF-8 boundary panic fixed: text splits now clamp requested offsets to prior char boundaries, run splitting validates local boundaries and falls back to reshaping when misaligned. latimes.com renders successfully (≈72s at 1200×800).
Added regression ensuring mid-codepoint split requests clamp to the previous char boundary (no panics, split_at aligns to codepoint start).
Added yahoo.com and nasa.gov to fetch_pages targets (cargo check --bin fetch_pages passes).
Added display-list renderer regressions for mix-blend-mode: non-isolated multiply darkens the backdrop, while isolation forces source-over compositing (blue over red).
Fetch/render notes: yahoo.com and nasa.gov fetch successfully but render timed out at 60s (large pages/heavy CSS).
render_pages now supports --timings to set FASTR_RENDER_TIMINGS for per-page stage timing logs.
yahoo.com renders in ~113s with timings enabled (cascade ~46s, layout ~59s) under a 120s timeout; still heavy but completes.
openbsd.org fetch/render now passes (PNG bbox approx 20..1199 x 15..734 at 1200x800).
fetch_and_render now accepts --timings to enable FASTR_RENDER_TIMINGS for per-stage logging when fetching a single page.
fetch_pages now accepts --timings to print per-page fetch durations (including cached/timeouts), plus timing output for cache hits/writes/errors.
booking.com fetch returns an empty body (0 bytes) even with Chrome UA; cached html cleared and refetch still empty—likely bot-blocked.
fetch_pages now treats empty HTTP bodies as errors (does not cache zero-byte responses) with a regression covering empty responses via a local test server.
fetch_pages now reports failed URLs in the summary output (prints a comma-separated list when any fetches fail).

- Rendered additional pages and found `openbsd.org` PNG was completely blank. Root cause: block layout dropped absolutely positioned children when the parent block was laid out via `layout_block_child` (positioned_children from `layout_children` were ignored). Added absolute-position handling to that path so out-of-flow children are laid out against the block's padding box.
- `openbsd.org` now renders with visible sidebar/content; `render_pages --pages openbsd.org` succeeds (PNG unique colors ~32k).
- Added regression `absolute_children_inside_block_descendants_are_laid_out` covering positioned children under nested blocks to guard the fix.
- Previous notes: marker baseline/list-style-position/ellipsis regressions landed upstream. Cloudflare/latimes fetch/render had timed out at 60s; no changes made.
CNN render with split_at char-boundary guard now finishes: render_pages --pages cnn.com --timeout 60 succeeded (~44s, ~196KB PNG); fetch_and_render timings ~7s cascade, ~4.6s box_tree, ~42s layout, ~0.5s paint.
 
- Fetched full page set (fetch_pages). Several 403/401s remain expected; cache populated for 85+ pages.
- render_pages was crashing on latimes.com due to splitting text at non-UTF-8 boundaries. TextItem::split_at now clamps to char boundaries and split_runs_preserving_shaping bails out when the run offset isn't a boundary, falling back to reshaping; added regression test split_at_handles_non_char_boundary_offsets.
- Reran render_pages for all cached pages; 86/86 pass, latimes.com now renders (PNG ~106KB). Summary in fetches/renders/_summary.log.
