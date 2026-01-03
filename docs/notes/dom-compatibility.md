# DOM compatibility mode

FastRender parses HTML without mutating author markup by default. Some pages expect
their bootstrap JavaScript to flip classes like `no-js` → `js-enabled` or to add
visibility gates such as `jsl10n-visible`; without scripting those class flips never
fire.

When a static render needs to mirror those initializations, enable compatibility mode:

- `FastRenderConfig::with_dom_compat_mode(DomCompatibilityMode::Compatibility)` for the
  high-level API
- `DomParseOptions::compatibility()` when calling `dom::parse_html_with_options`
- CLIs: pass `--dom-compat compat` (and optionally `--compat-profile site`) to
  `fetch_and_render`, `render_pages`, `pageset_progress` (run/worker),
  `bundle_page` (fetch/render), or `inspect_frag`.

Compatibility mode applies a small, generic set of mutations:

- If the `<html>` element has a `no-js` class, replace it with `js-enabled`
- Add `jsl10n-visible` to `<html>` and `<body>` when missing
- If an `<img>` or `<source>` element is missing `src`/`srcset` (or uses a common
  placeholder `src` like `about:blank` or a 1×1 transparent GIF `data:` URL),
  copy common lazy-load `data-*` attributes (`data-src`, `data-lazy-src`,
  `data-gl-src`, `data-srcset`, etc.) into the real attributes so CSS like
  `img:not([src]):not([srcset]) { visibility: hidden }` doesn't permanently
  suppress the image in static renders.

Leaving compatibility mode at `DomCompatibilityMode::Standard` (the default) keeps the
parsed DOM spec-faithful.
