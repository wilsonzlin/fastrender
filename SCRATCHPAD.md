Render pipeline now decodes cached HTML with proper charset sniffing: fetch_pages stores the response Content-Type alongside each cached HTML (.html.meta), render_pages decodes bytes via the shared html::encoding helper (BOM/header/meta/default Windows-1252), and fetch_and_render reuses the shared decoder instead of its local copy. cloudflare.com and latimes.com timeouts still outstanding from earlier notes.
Inline split guard: TextItem::split_at now bails out on non-char-boundary offsets (avoiding UTF-8 slice panics) and a regression covers mid-emoji splits; cleaned an unused MixBlendMode import. Marker baseline/list-style-position/ellipsis regressions landed upstream.
fetch_pages cache writes are now centralized: HTML caching writes optional .html.meta sidecars via a helper, and tests cover meta persistence/removal; charset sniffing coverage unaffected.
fetch_and_render now reads .meta sidecars for file:// HTML inputs (e.g., cached pages) and passes the cached Content-Type through decode_html_bytes; added a regression ensuring Shift-JIS HTML decodes via the meta charset.
Aligned fetcher user-agents: exported DEFAULT_USER_AGENT in resource.rs, fetch_and_render now sends it on HTTP requests, and fetch_pages uses the same Chrome-like UA instead of the old compatible string. Trimmed an unused MixBlendMode import in display_list_renderer_test.
Deduplicated DEFAULT_USER_AGENT in resource.rs after rebase to avoid duplicate const definitions.
Hyphenation breaks now filter out non-char-boundary offsets before use, and a regression covers dropping a break inside a multibyte emoji.
cloudflare.com fetch/render timed out at 60s; no changes made
latimes.com fetch/render timed out at 60s; no changes made
latimes.com fetch/render timed out at 60s; no changes made (duplicate notes removed)
HTML parsing now disables scripting so <noscript> fallbacks are parsed/renderable; regression added for noscript content.
