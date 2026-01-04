Synthetic WPT-like test data used by `import_wpt` tests.

The layout mirrors a tiny subset of upstream WPT with:
- a reftest under `css/simple/` that depends on `/resources` and a support stylesheet
- a mismatch reftest under `css/simple/` with `.html.ini` sidecars (and whitespace-separated `rel` tokens)
- absolute-origin resource URL fixtures under `css/simple/` (`http(s)://web-platform.test/...`)
- a `srcset` rewriter fixture under `css/simple/`
- a CSS `@namespace url("http://www.w3.org/...")` fixture under `css/simple/` (namespace URIs should not trip offline validation)
- a visual test under `html/standalone/` that pulls in nested CSS with `@import`
- a set of network/url validation fixtures under `html/network/`:
  - `external-url.html`: simple `src="https://example.com/..."` rejection
  - `help-link.html`: `rel=help` links are allowed in non-strict mode (strict-offline still rejects)
  - `srcset-external.html`: `srcset="https://example.com/..."` rejection
  - `unquoted-external.html`: unquoted `src=https://example.com/...` rejection
  - `text-url.html`: strict-offline rejection of network-looking strings outside fetch contexts
  - `imagesrcset.html`: `imagesrcset="...web-platform.test..."` rewrite
  - `imagesrcset-external.html`: `imagesrcset="https://example.com/..."` rejection
  - `data-url.html`: data: URLs containing `http://...` substrings in payloads (should not trigger validation)
  - `data-attrs.html`: `data-src`/`data-srcset` containing network URLs (should not trigger non-strict validation)
  - `mailto.html`: non-fetchable `mailto:` links
  - `poster.html`: `poster="/resources/..."` rewrite
  - `object-data.html`: `data="/resources/..."` rewrite
